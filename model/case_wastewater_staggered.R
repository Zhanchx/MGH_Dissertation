
############# CASE DATA AND WASTEWATER DATA COMBINED #################################
# case data from Jan 2021, waste-water from June ------------------------------

rm(list=ls())

library(here)
library(epidemia)
library(tidyverse)
library(matrixStats)
options(mc.cores=parallel::detectCores())

# reading saved data
region_data_w <- read_csv(here('../data/processed_water_data.csv'))
region_data_c <- read_csv(here('../data/regional_uk_cases_jan.csv'))

region_data_w$area_name<- gsub("Yorkshire and The Humber", "Yorkshire and Humber", region_data_w$area_name)

region_data<- left_join(region_data_c, region_data_w, by = c("area_name", "date"))

region_data<- region_data|>
  select(date, area_name, week.x, cases_week, week.y, signal)


# calculating priors for initial infection seeding ------------------------

region_seeding<- region_data|>
  select(date, area_name, cases_week)|>
  filter(date== "2021-01-17" | date==  "2021-01-24" |date== "2021-01-31")|>
  group_by(area_name)|>
  mutate(threewk_mean_infection = mean(cases_week))|>
  filter(date == "2021-01-17")

region_seeding$IAR <- 0.19
region_seeding$daily_mean<- region_seeding$threewk_mean_infection/7

# infections calculated from mean of cases for the first three weeks/ IAR 
region_seeding$seeding_infections<- round(region_seeding$daily_mean/
                                            region_seeding$IAR, digits = 0)

# -------------------------------------------------------------------------

pops <-  read_csv(here('../data/modified_population.csv'))

# region to run analysis for
region_name <- 'Yorkshire and Humber'

get_pop_region <- function(region){
  if (region == "Yorkshire and The Humber"){
    return("Yorkshire And Humber")
  } else {
    return(region)
  }
}
pop_region_name <- get_pop_region(region_name)

seed <- 21313578

# obtaining the prior for the seeding for that region assuming Rt= 1 at the beginning of model 
region_seeding_mean1<- region_seeding[region_seeding$area_name == region_name,]$seeding_infections
region_seeding_mean<- (region_seeding_mean1)^-1

# sub-setting data for specific region and adding population
data_area <- 
  region_data %>%
  filter(area_name == region_name)

data_area<- data_area|>
  rename(week = week.x)|>
  rename(week.s = week.y)

# adding 4 extra weeks
data_area <- 
  data_area %>%
  mutate(date = as.Date(date)) %>%  # Ensure date column is of Date type
  summarise(min_date = min(date))%>%
  mutate(new_dates = list(seq(min_date - 28, min_date - 1, by = "days"))) %>%
  unnest(new_dates) %>%
  select(date = new_dates) %>%
  mutate(area_name = region_name, signal = NA, week = 1) %>%
  bind_rows(data_area, .) %>%
  mutate(pop = pull(filter(pops, area_name == region_name),pop_2018),
         iar = 0.19
  ) %>%
  arrange(date, week)

# reading i2o rates
i2o_rates <- read_rds(here('../data/i2o_rates.rds'))
IFR_sd <- pull(filter(i2o_rates, type=='IFR_sd'), value)
IAR_sd <-pull(filter(i2o_rates, type=='IAR_sd'), value)

# function to change any daily distribution to weekly
i2o2week <- function(i2o)
  rowSums(sapply(0:6, function (k) c(rep(0,k),i2o,rep(0,6-k))))

#conversion function from mean and standard deviation to gamma distribution 
convert_mean_cv_to_gamma <- function(mean, cv) {
  shape <- 1 / (cv^2)
  rate <- shape / mean
  return(list(shape = shape, rate = rate))
}
convert_mean_sd_to_gamma <- function(mean, sd) {
  shape <- (mean / sd)^2
  rate <- mean / (sd^2)
  return(list(shape = shape, rate = rate))
}

#infection to onset distribution and gastrointestingal shedding distribution 
mean_inf_onset <- 5.1
cv_inf_onset <- 0.86
mean_onset_shedding <- 6.7 #6# 6.7
sd_onset_shedding <- 6.5 #7
shape_rate_inf_onset <- convert_mean_cv_to_gamma(mean_inf_onset, cv_inf_onset)
shape_rate_onset_shedding <- convert_mean_sd_to_gamma(mean_onset_shedding, sd_onset_shedding)

x1 <- rgamma(1e6, shape_rate_inf_onset$shape, shape_rate_inf_onset$rate) # inf to onset
x2<- rgamma(1e6, shape_rate_onset_shedding$shape, shape_rate_onset_shedding$rate) # onset to shedding
ecdf.saved<- ecdf(x1+x2)
time_days <- 45

#convolution 
f<- rep(0, time_days)
convolution = function(u) (ecdf.saved(u))
f[1]= (convolution(1.5)-convolution(0))
for(i in 2: time_days){
  f[i] = (convolution(i+.5)- convolution(i-.5))
}

# observation model for cases, further discussion in supplementary file 
cases_week <- epiobs(formula=cases_week ~ 1 + rw(time = week, prior_scale = 0.05),
                     link="logit",
                     family="neg_binom", 
                     prior_aux = rstanarm::normal(location=5,2),
                     prior_intercept = normal(0,0.15), 
                     i2o=i2o2week(c(0,0,0,rep(1/10,10)) 
                     )
)

# observation model for wastewater, further discussion in supplementary file 
signal <- epiobs(formula=signal ~ 1,
                 link="logit",
                 i2o=i2o2week(f) 
)

# transmission model, discussion available in supplementary file 
rt <- epirt(formula=R(area_name,date) ~ rw(time = week, prior_scale = 0.2))

# infection model, discussion available in supplementary file 
inf <- epiinf(gen = EuropeCovid$si, seed_days=21L, 
              pop_adjust = TRUE, 
              pops = pop,
              prior_seeds = hexp(exponential(region_seeding_mean)))

# fitting to data
fm_wc <- epim(rt = rt, inf = inf, obs = list(cases_week, signal), data = data_area,
              iter = 4e3, control = list(max_treedepth = 15), seed=seed)

# saving everything
saveRDS(fm_wc, here(paste("../fits/","fm_wc_",region_name, ".rds",sep="")))

plot_obs(fm_wc, type = "signal", levels = c(50, 89))
ggsave(here(paste("../output/","fm_wc_signal_",region_name, ".png",sep="")), width=14, height = 7)
ggsave(here(paste("../output/","fm_wc_signal_",region_name, ".svg",sep="")), width=14, height = 7)

plot_obs(fm_wc, type = "cases_week", levels = c(50, 89))
ggsave(here(paste("../output/","fm_wc_cases_week_",region_name, ".png",sep="")), width=14, height = 7)
ggsave(here(paste("../output/","fm_wc_cases_week_",region_name, ".svg",sep="")), width=14, height = 7)

plot_rt<- plot_rt(fm_wc, levels = c(50, 89))
ggsave(here(paste("../output/","fm_rt_wc_",region_name, ".png",sep="")), width=14, height = 7)
ggsave(here(paste("../output/","fm_rt_wc_",region_name, ".svg",sep="")), width=14, height = 7)

