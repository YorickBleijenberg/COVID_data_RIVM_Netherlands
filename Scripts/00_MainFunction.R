
###   Cumulatieve getallen

##  Connect to online:
## https://happygitwithr.com/

## https://www.r-graph-gallery.com/233-add-annotations-on-ggplot2-chart.html
## https://rdrr.io/cran/ggplot2/man/scale_gradient.html
## https://aosmith.rbind.io/2019/10/14/background-color_gradient/


####Check LSPC update
LCPS_datafeed<-read.csv("https://lcps.nu/wp-content/uploads/covid-19.csv",sep=",")
LCPS_datafeed$Datum <- as.Date(LCPS_datafeed$Datum ,format="%d-%m-%Y")
LCPS_datafeed <- LCPS_datafeed[order(LCPS_datafeed$Datum),]
last(LCPS_datafeed,2)

## Nieuwe maatregelen toevoegen - bieb dicht - 4 november

#### ECDC - ANIMATION
#### R ANIMATION
###  auto upload to git
###  set-up remote
##### vakantie effect per veiligheidsregio  #####
#library(ggrepel)

#### check selected dates ####
rm(list=ls())

library(jsonlite)
library(rtweet)
library(tidyverse)
library(zoo)
library(RcppRoll)
library(data.table)
library(scales)
library(lubridate)
source("C:\\Rdir\\Rscripts\\geom_stream.R")
source("C:\\Rdir\\Rscripts\\03A_TwitterAuthentication.r")  ## source("C:\\Rdir\\Rscripts\\03test_TwitterAuthentication.r")

#### some constants for the update ####

editionname <- "ice-skating-during-a-pandemic"
editienaam  <- "schaatsen-in-coronatijd"
#number.in.DE <- "0"

#### Start  ####

dirname <- paste("data/",Sys.Date(),sep = "")
dir.create(dirname)

#### update at 14:00h ####
source("C:\\Rdir\\Rscripts\\01_lcps_data_graph.R")



#### update at 15:15 ####
source("C:\\Rdir\\Rscripts\\02_RIVM_data_importer_and_archiver.R")

source("C:\\Rdir\\Rscripts\\04_DataPrep.R")

print(Working_Set)

source("C:\\Rdir\\Rscripts\\05_DailyPlots.R")

source("C:\\Rdir\\Rscripts\\06_WoW_DoD_plots.R")

source("C:\\Rdir\\Rscripts\\07_Age-heatmap_barchart.R")

source("C:\\Rdir\\Rscripts\\07A_Age-heatmap_barchart-hosp.R")

source("C:\\Rdir\\Rscripts\\08_caseType_diff.R")
   
source("C:\\Rdir\\Rscripts\\08b_caseType.R")

source("C:\\Rdir\\Rscripts\\09_WeekOfDeath-plot.R")

source("C:\\Rdir\\Rscripts\\09A_growth.R")

source("C:\\Rdir\\Rscripts\\12_cities.R")

source("C:\\Rdir\\Rscripts\\12_cities_Lansingerland.R")  # Lansingerland

source("C:\\Rdir\\Rscripts\\14_provincies.R")

#source("C:\\Rdir\\Rscripts\\17_Herstvakantie-effect.R")

source("C:\\Rdir\\Rscripts\\20_carehomes.R")

source("C:\\Rdir\\Rscripts\\22_prediction.R")

source("C:\\Rdir\\Rscripts\\29_coronamelder.R")

source("C:\\Rdir\\Rscripts\\30_weekly_numbers.R")

source("C:\\Rdir\\Rscripts\\33_christmas_deaths.R")

source("C:\\Rdir\\Rscripts\\35_vaccinated.R")

###


#### check correct last tweet ####



 source("C:\\Rdir\\Rscripts\\10_EN_TwitterTread.R")

 source("C:\\Rdir\\Rscripts\\26_Municipality.R")

##upload todays pictures"

source("C:\\Rdir\\Rscripts\\18_Dashboard_daily_pos_rate.R")

## source("C:\\Rdir\\Rscripts\\10_TwitterTread.R")

source("C:\\Rdir\\Rscripts\\25_Veiligheidsregios_insight.R")




