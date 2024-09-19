
# processing latent infections - cases and ww -----------------------------

rm(list=ls())
library(epidemia)
library(dplyr)
library(tidyverse)
getwd()

fit<- readRDS("../fits/rds files/wastewater_cases/fm_wc_East Midlands.rds")

save_lat_inf_plot<- function(data_directory, plot_directory, region){
  
  region_title <- sub("fm_wc_", "", region)
  
  data_path<- paste0(data_directory, region, ".rds")
  
  fit<- readRDS(data_path)
  lat_inf_plot<- plot_infections(fit, transform= TRUE, 
                                 date_breaks = "4 weeks")+
                  scale_y_continuous(limits = c(0, 9 *10^4), 
                                     breaks = seq(0, 90000, by = 1*10^4)) +
                  labs(title = NULL, subtitle = region_title) +
                  theme(plot.title = element_blank())
  
  output_file<- paste0(plot_directory, region, ".png")
  png(filename = output_file)
  plot(lat_inf_plot)
  dev.off()
}


all_files<- list.files(path = "../fits/rds files/wastewater_cases/", pattern = ".rds", 
                       all.files = TRUE, full.names = FALSE)
data_directory<-"../fits/rds files/wastewater_cases/"
plot_directory<-"../output/latent_infections//"

for (pth in all_files){
  reg<-tools::file_path_sans_ext(pth)
  print(reg)
  save_lat_inf_plot(data_directory,plot_directory,reg)
}
