rm(list=ls())
library(epidemia)
library(tools)
library(dplyr)
library(tidyverse)
getwd()


# processing rt  ----------------------------------------------------------

# East Midlands -----------------------------------------------------------
rt_fit_em<- readRDS("../fits/rds files/wastewater_cases/fm_wc_East Midlands.rds")
rt_plot_em<- plot_rt(rt_fit_em, step = T, levels = c(95))
rt_plot_em

rt_table_em<- rt_plot_em$data|>
  select(date, lower, upper)|>
  rename(east_mid = lower)


# East England  -----------------------------------------------------------

rt_fit_ee<- readRDS("../fits/rds files/wastewater_cases/fm_wc_East of England.rds")
rt_plot_ee<- plot_rt(rt_fit_ee, step = T, levels = c(95))
rt_plot_ee

rt_table_ee<- rt_plot_ee$data|>
  select(date, lower, upper)|>
  rename(east_eng= lower)

# london ------------------------------------------------------------------

rt_fit_ldn<- readRDS("../fits/rds files/wastewater_cases/fm_wc_London.rds")
rt_plot_ldn<- plot_rt(rt_fit_ldn, step = T, levels = c(95))
rt_plot_ldn

rt_table_ldn<- rt_plot_ldn$data|>
  select(date, lower, upper)|>
  rename(london = lower)

# north east  ------------------------------------------------------------------

rt_fit_ne<- readRDS("../fits/rds files/wastewater_cases/fm_wc_North East.rds")
rt_plot_ne<- plot_rt(rt_fit_ne, step = T, levels = c(95))
rt_plot_ne

rt_table_ne<- rt_plot_ne$data|>
  select(date, lower, upper)|>
  rename(north_east = lower)

# north west  ------------------------------------------------------------------

rt_fit_nw<- readRDS("../fits/rds files/wastewater_cases/fm_wc_North West.rds")
rt_plot_nw<- plot_rt(rt_fit_nw, step = T, levels = c(95))
rt_plot_nw

rt_table_nw<- rt_plot_nw$data|>
  select(date, lower, upper)|>
  rename(north_west = lower)

# south east  ------------------------------------------------------------------

rt_fit_se<- readRDS("../fits/rds files/wastewater_cases/fm_wc_South East.rds")
rt_plot_se<- plot_rt(rt_fit_se, step = T, levels = c(95))
rt_plot_se

rt_table_se<- rt_plot_se$data|>
  select(date, lower, upper)|>
  rename(south_east = lower)

# south west  ------------------------------------------------------------------

rt_fit_sw<- readRDS("../fits/rds files/wastewater_cases/fm_wc_South West.rds")
rt_plot_sw<- plot_rt(rt_fit_sw, step = T, levels = c(95))
rt_plot_sw

rt_table_sw<- rt_plot_sw$data|>
  select(date, lower, upper)|>
  rename(south_west = lower)

# west mid  ------------------------------------------------------------------

rt_fit_wm<- readRDS("../fits/rds files/wastewater_cases/fm_wc_West Midlands.rds")
rt_plot_wm<- plot_rt(rt_fit_wm, step = T, levels = c(95))
rt_plot_wm

rt_table_wm<- rt_plot_wm$data|>
  select(date, lower, upper)|>
  rename(west_mid = lower)

# yorkshire and humber   ------------------------------------------------------------------

rt_fit_yh<- readRDS("../fits/rds files/wastewater_cases/fm_wc_Yorkshire and Humber.rds")
rt_plot_yh<- plot_rt(rt_fit_yh, step = T, levels = c(95))
rt_plot_yh

rt_table_yh<- rt_plot_yh$data|>
  select(date, lower, upper)|>
  rename(york_hum = lower)


# making table  -----------------------------------------------------------

rt_table_em$east_eng<- rt_table_ee$east_eng
rt_table_em$london<- rt_table_ldn$london
rt_table_em$north_east<- rt_table_ne$north_east
rt_table_em$north_west<- rt_table_nw$north_west
rt_table_em$south_east<- rt_table_se$south_east
rt_table_em$south_west<- rt_table_sw$south_west
rt_table_em$west_mid<- rt_table_wm$west_mid
rt_table_em$york_hum<- rt_table_yh$york_hum


max_values<- rt_table_em|>
  summarise(
    East_Mid_Value = max(east_mid, na.rm = TRUE),
    East_Mid_Date = date[which.max(east_mid)],
    
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
