rm(list=ls())
library(epidemia)
library(tools)
library(dplyr)
library(tidyverse)
getwd()


# East Midlands -----------------------------------------------------------
em_fit<- readRDS("../fits/rds files/wastewater_cases/fm_wc_East Midlands.rds")

em_linf_plot <- plot_infections(em_fit, type = "cases_week", transform = TRUE,
                            levels = c(0, 30, 60, 90))

em_linf_table<- em_linf_plot$data|>
  select(date, lower)|>
  rename(East_Mid = lower) 

em_linf_plot

# East of England  --------------------------------------------------------
ee_fit<- readRDS("../fits/rds files/wastewater_cases/fm_wc_East of England.rds")

ee_linf_plot <- plot_infections(ee_fit, type = "cases_week", transform = TRUE,
                            levels = c(0, 30, 60, 90))

ee_linf_table<- ee_linf_plot$data|>
  select(date, lower)|>
  rename(East_Eng = lower)

# London ------------------------------------------------------------------
ldn_fit<- readRDS("../fits/rds files/wastewater_cases/fm_wc_London.rds")

ldn_linf_plot <- plot_infections(ldn_fit, type = "cases_week", transform = TRUE,
                             levels = c(0, 30, 60, 90))

ldn_linf_table<- ldn_linf_plot$data|>
  select(date, lower)|>
  rename(london = lower)

# North East --------------------------------------------------------------
ne_fit<- readRDS("../fits/rds files/wastewater_cases/fm_wc_North East.rds")

ne_linf_plot <- plot_infections(ne_fit, type = "cases_week", transform = TRUE,
                            levels = c(0, 30, 60, 90))

ne_linf_table<- ne_linf_plot$data|>
  select(date, lower)|>
  rename(north_east = lower)

# north west --------------------------------------------------------------
nw_fit<- readRDS("../fits/rds files/wastewater_cases/fm_wc_North West.rds")

nw_linf_plot <- plot_infections(nw_fit, type = "cases_week", transform = TRUE,
                            levels = c(0, 30, 60, 90))

nw_linf_table<- nw_linf_plot$data|>
  select(date, lower)|>
  rename(north_west = lower)

# South East --------------------------------------------------------------
se_fit<- readRDS("../fits/rds files/wastewater_cases/fm_wc_South East.rds")

se_linf_plot <- plot_infections(se_fit, type = "cases_week", transform = TRUE,
                            levels = c(0, 30, 60, 90))

se_linf_table<- se_linf_plot$data|>
  select(date, lower)|>
  rename(south_east = lower)

# South West --------------------------------------------------------------
sw_fit<- readRDS("../fits/rds files/wastewater_cases/fm_wc_South West.rds")

sw_linf_plot <- plot_infections(sw_fit, type = "cases_week", transform = TRUE,
                            levels = c(0, 30, 60, 90))

sw_linf_table<- sw_linf_plot$data|>
  select(date, lower)|>
  rename(south_west = lower)

# West Midlands -----------------------------------------------------------
wm_fit<- readRDS("../fits/rds files/wastewater_cases/fm_wc_West Midlands.rds")

wm_linf_plot <- plot_infections(wm_fit, type = "cases_week", transform = TRUE,
                            levels = c(0, 30, 60, 90))

wm_linf_table<- wm_linf_plot$data|>
  select(date, lower)|>
  rename(west_mid = lower)

# Yorkshire and Humber  ---------------------------------------------------
yh_fit<- readRDS("../fits/rds files/wastewater_cases/fm_wc_Yorkshire and Humber.rds")

yh_linf_plot <- plot_infections(yh_fit, type = "cases_week", transform = TRUE,
                            levels = c(0, 30, 60, 90))

yh_linf_table<- yh_linf_plot$data|>
  select(date, lower)|>
  rename(yorkshire_humber = lower)


# making table  -----------------------------------------------------------

em_linf_table$east_eng<- ee_linf_table$East_Eng
em_linf_table$london<- ldn_linf_table$london
em_linf_table$north_east<- ne_linf_table$north_east
em_linf_table$north_west<- nw_linf_table$north_west 
em_linf_table$south_east<- se_linf_table$south_east  
em_linf_table$south_west<- sw_linf_table$south_west  
em_linf_table$west_mid<- wm_linf_table$west_mid  
em_linf_table$york_hum<- yh_linf_table$yorkshire_humber  

em_linf_summer_table<- em_linf_table|>
  filter(date >= "2021-04-01" & date <= "2021-10-01")

max_values<- em_linf_summer_table|>
    summarise(
    East_Mid_Value = max(East_Mid, na.rm = TRUE),
    East_Mid_Date = date[which.max(East_Mid)],
    
    East_Eng_Value = max(east_eng, na.rm = TRUE),
    East_Eng_Date = date[which.max(east_eng)],
    
    London_Value = max(london, na.rm = TRUE),
    London_Date = date[which.max(london)],
    
    North_East_Value = max(north_east, na.rm = TRUE),
    North_East_Date = date[which.max(north_east)],
    
    North_West_Value = max(north_west, na.rm = TRUE),
    North_West_Date = date[which.max(north_west)],
    
    South_East_Value = max(south_east, na.rm = TRUE),
    South_East_Date = date[which.max(south_east)],
    
    South_West_Value = max(south_west, na.rm = TRUE),
    South_West_Date = date[which.max(south_west)],
    
    West_Mid_Value = max(west_mid, na.rm = TRUE),
    West_Mid_Date = date[which.max(west_mid)],
    
    York_Hum_Value = max(york_hum, na.rm = TRUE),
    York_Hum_Date = date[which.max(york_hum)]
  )
