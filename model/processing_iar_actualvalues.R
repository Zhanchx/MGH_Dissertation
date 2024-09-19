rm(list=ls())
library(epidemia)
library(tools)
library(dplyr)
library(tidyverse)
getwd()

# East Midlands -----------------------------------------------------------
  em_fit<- readRDS("../fits/rds files/wastewater_cases/fm_wc_East Midlands.rds")

  em_iar_plot <- plot_linpred(em_fit, type = "cases_week", transform = TRUE,
                           levels = c(0, 30, 60, 90))
 
  em_iar_table<- em_iar_plot$data|>
    select(date, lower)|>
    rename(East_Mid = lower) 

    # East of England  --------------------------------------------------------
  ee_fit<- readRDS("../fits/rds files/wastewater_cases/fm_wc_East of England.rds")
  
  ee_iar_plot <- plot_linpred(ee_fit, type = "cases_week", transform = TRUE,
                              levels = c(0, 30, 60, 90))
  
  ee_iar_table<- ee_iar_plot$data|>
    select(date, lower)|>
    rename(East_Eng = lower)

# London ------------------------------------------------------------------
  ldn_fit<- readRDS("../fits/rds files/wastewater_cases/fm_wc_London.rds")
  
  ldn_iar_plot <- plot_linpred(ldn_fit, type = "cases_week", transform = TRUE,
                              levels = c(0, 30, 60, 90))
  
  ldn_iar_table<- ldn_iar_plot$data|>
    select(date, lower)|>
    rename(london = lower)

# North East --------------------------------------------------------------
  ne_fit<- readRDS("../fits/rds files/wastewater_cases/fm_wc_North East.rds")
  
  ne_iar_plot <- plot_linpred(ne_fit, type = "cases_week", transform = TRUE,
                               levels = c(0, 30, 60, 90))
  
  ne_iar_table<- ne_iar_plot$data|>
    select(date, lower)|>
    rename(north_east = lower)
  
# north west --------------------------------------------------------------
  nw_fit<- readRDS("../fits/rds files/wastewater_cases/fm_wc_North West.rds")
  
  nw_iar_plot <- plot_linpred(nw_fit, type = "cases_week", transform = TRUE,
                              levels = c(0, 30, 60, 90))
  
  nw_iar_table<- nw_iar_plot$data|>
    select(date, lower)|>
    rename(north_west = lower)

# South East --------------------------------------------------------------
  se_fit<- readRDS("../fits/rds files/wastewater_cases/fm_wc_South East.rds")
  
  se_iar_plot <- plot_linpred(se_fit, type = "cases_week", transform = TRUE,
                              levels = c(0, 30, 60, 90))
  
  se_iar_table<- se_iar_plot$data|>
    select(date, lower)|>
    rename(south_east = lower)

# South West --------------------------------------------------------------
  sw_fit<- readRDS("../fits/rds files/wastewater_cases/fm_wc_South West.rds")
  
  sw_iar_plot <- plot_linpred(sw_fit, type = "cases_week", transform = TRUE,
                              levels = c(0, 30, 60, 90))
  
  sw_iar_table<- sw_iar_plot$data|>
    select(date, lower)|>
    rename(south_west = lower)

# West Midlands -----------------------------------------------------------
  wm_fit<- readRDS("../fits/rds files/wastewater_cases/fm_wc_West Midlands.rds")
  
  wm_iar_plot <- plot_linpred(wm_fit, type = "cases_week", transform = TRUE,
                              levels = c(0, 30, 60, 90))
  
  wm_iar_table<- wm_iar_plot$data|>
    select(date, lower)|>
    rename(west_mid = lower)

# Yorkshire and Humber  ---------------------------------------------------
  yh_fit<- readRDS("../fits/rds files/wastewater_cases/fm_wc_Yorkshire and Humber.rds")
  
  yh_iar_plot <- plot_linpred(yh_fit, type = "cases_week", transform = TRUE,
                              levels = c(0, 30, 60, 90))
  
  yh_iar_table<- yh_iar_plot$data|>
    select(date, lower)|>
    rename(yorkshire_humber = lower)
  

# making table  -----------------------------------------------------------

  em_iar_table$east_eng<- ee_iar_table$East_Eng
  em_iar_table$london<- ldn_iar_table$london
  em_iar_table$north_east<- ne_iar_table$north_east
  em_iar_table$north_west<- nw_iar_table$north_west 
  em_iar_table$south_east<- se_iar_table$south_east  
  em_iar_table$south_west<- sw_iar_table$south_west  
  em_iar_table$west_mid<- wm_iar_table$west_mid  
  em_iar_table$york_hum<- yh_iar_table$yorkshire_humber  

# trough ------------------------------------------------------------------

  min_values_with_dates <- em_iar_table %>%
    summarise(
      East_Mid_Value = min(East_Mid, na.rm = TRUE),
      East_Mid_Date = date[which.min(East_Mid)],
      
      East_Eng_Value = min(east_eng, na.rm = TRUE),
      East_Eng_Date = date[which.min(east_eng)],
      
      London_Value = min(london, na.rm = TRUE),
      London_Date = date[which.min(london)],
      
      North_East_Value = min(north_east, na.rm = TRUE),
      North_East_Date = date[which.min(north_east)],
      
      North_West_Value = min(north_west, na.rm = TRUE),
      North_West_Date = date[which.min(north_west)],
      
      South_East_Value = min(south_east, na.rm = TRUE),
      South_East_Date = date[which.min(south_east)],
      
      South_West_Value = min(south_west, na.rm = TRUE),
      South_West_Date = date[which.min(south_west)],
      
      West_Mid_Value = min(west_mid, na.rm = TRUE),
      West_Mid_Date = date[which.min(west_mid)],
      
      York_Hum_Value = min(york_hum, na.rm = TRUE),
      York_Hum_Date = date[which.min(york_hum)]
    )