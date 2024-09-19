rm(list=ls())
library(epidemia)
library(tools)
getwd()

save_IAR_plot<- function(data_directory, plot_directory, region){
  
  data_path <- paste0(data_directory, region, ".rds") 
                      
  fit<- readRDS(data_path)
  iar_plot <- plot_linpred(fit, type = "cases_week", transform = TRUE,
                           date_breaks = "2 weeks",) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1), 
                       minor_breaks = seq(0, 1, by = 0.02)) +
    ylab("") +
    theme(legend.position = "none")
  
  output_file<- paste0(plot_directory, region, ".png")
  png(filename = output_file)
  plot(iar_plot)
  dev.off()

}

all_files<- list.files(path = "../fits/rds files/wastewater_cases/", pattern = ".rds", 
                        all.files = TRUE, full.names = FALSE)
data_directory<-"../fits/rds files/wastewater_cases/"
plot_directory<-"../output/IAR/"

for (pth in all_files){
  reg<-tools::file_path_sans_ext(pth)
  print(reg)
  save_IAR_plot(data_directory,plot_directory,reg)
}

