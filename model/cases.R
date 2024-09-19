
# fitting cases only, from Jan 2021 - Dec 2021 ----------------------------

rm(list=ls())

library(here)
library(epidemia)
library(tidyverse)
library(matrixStats)
library(dplyr)
options(mc.cores=parallel::detectCores())

# reading saved data
region_data <- read_csv('../data/regional_uk_cases_jan.csv')
pops <-  read_csv("../data/modified_population.csv")

# obtaining infection seeding priors --------------------------------------

region_seeding<- region_data|>
  select(area_name, date, cases_week)|>
  filter(date == "2021-01-17"| date== "2021-01-24" | date == "2021-01-31")|>
  group_by(area_name)|>
  mutate(threewk_mean_cases = mean(cases_week))|>
  filter(date == "2021-01-17")

region_seeding$IAR <- 0.19 
region_seeding$daily_mean<- region_seeding$threewk_mean_cases/7
region_seeding$seeding_infections<- round(region_seeding$daily_mean/region_seeding$IAR, digits = 0)

# # -----------------------------------------------------------------------

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

#calculating seeding priors assuming Rt=1 at the beginning of the model 
region_seeding_mean1<- region_seeding[region_seeding$area_name == region_name,]$seeding_infections
region_seeding_mean<- (region_seeding_mean1)^-1

# sub-setting data for specific region and adding population
data_area <- 
  region_data %>%
  filter(area_name == region_name) %>%
  select(area_name, date, cases_week, week)


# adding 4 extra weeks
data_area <- 
  data_area %>%
  mutate(date = as.Date(date)) %>%  # Ensure date column is of Date type
  summarise(min_date = min(date)) %>%
  mutate(new_dates = list(seq(min_date - 28, min_date - 1, by = "days"))) %>%
  unnest(new_dates) %>%
  select(date = new_dates) %>%
  mutate(area_name = region_name, cases_week = NA, week = 1) %>%
  bind_rows(data_area, .) %>%
  mutate(pop = pull(filter(pops, area_name == region_name),pop_2018),
         iar = 0.19
  ) %>%
  arrange(date, week)

# reading i2o rates
i2o_rates <- read_rds("../data/i2o_rates.rds")
IFR_sd <- pull(filter(i2o_rates, type=='IFR_sd'), value)
IAR_sd <-pull(filter(i2o_rates, type=='IAR_sd'), value)

# function to change any daily distribution to weekly distribution 
i2o2week <- function(i2o)
  rowSums(sapply(0:6, function (k) c(rep(0,k),i2o,rep(0,6-k))))

# observation model, discussion available in supplementary file 
cases_week <- epiobs(formula=cases_week ~ 1 + rw(time = week, prior_scale = 0.05),
                     link="logit",
                     family="neg_binom",  
                     prior_aux = rstanarm::normal(location=5,2), 
                     prior_intercept = normal(0,0.15),  
                     i2o=i2o2week(c(0,0,0,rep(1/10,10))  
                     )
)
# transmission model, discussion available in supplementary file 
rt <- epirt(formula=R(area_name,date) ~ rw(time = week, prior_scale = 0.2))

# infection model, discussion available in supplementary file 
inf <- epiinf(gen = EuropeCovid$si, seed_days=21L, 
              pop_adjust = TRUE, 
              pops = pop,
              prior_seeds = hexp(exponential(region_seeding_mean))) 

# fitting to data, discussion available in supplementary file 
fm <- epim(rt = rt, inf = inf, obs = list(cases_week), data = data_area,
           iter = 4e3, control = list(max_treedepth = 15), seed=seed)


# saving everything
saveRDS(fm,here(paste("../fits/","fm_",region_name, ".rds",sep="")))
plot_obs(fm, type = "cases_week", levels = c(50, 89))
ggsave(here(paste("../output/","fm_cases_week_",region_name, ".png",sep="")), width=14, height = 7)
ggsave(here(paste("../output/","fm_cases_week_",region_name, ".svg",sep="")), width=14, height = 7)
plot_rt(fm, step = T, levels = c(50,89))
ggsave(here(paste("../output/","fm_rt_",region_name, ".png",sep="")), width=14, height = 7)
ggsave(here(paste("../output/","fm_rt_",region_name, ".svg",sep="")), width=14, height = 7)
