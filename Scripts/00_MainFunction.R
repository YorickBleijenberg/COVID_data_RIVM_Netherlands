

rm(list=ls())

library(jsonlite)
library(rtweet)
library(tidyverse)
library(zoo)
library(RcppRoll)
library(data.table)
library(scales)
library(lubridate)

#source("C:\\Rdir\\Rscripts\\geom_stream.R")
source("C:\\Rdir\\Rscripts\\store\\03A_TwitterAuthentication.r")



LCPS_datafeed<-read.csv("https://lcps.nu/wp-content/uploads/covid-19-datafeed.csv",sep=",")
LCPS_datafeed$Datum <- as.Date(LCPS_datafeed$Datum ,format="%d-%m-%Y")
LCPS_datafeed <- LCPS_datafeed[order(LCPS_datafeed$Datum),]
last(LCPS_datafeed,2)
last(LCPS_datafeed$Datum) == Sys.Date()


#### some constants for the update ####
beds_current_capacity <- 1007
editionname <- "trend-blijft-nog-even-dalen"
#inDE                  <- last(LCPS_datafeed$IC_Bedden_COVID_Internationaal,1)
#vaccine.edition.name <- "weer-geen-open-booster-vaccinatie-data"
#leeftijd             <- "1952"



#### Start  ####
dirname <- paste("data/",Sys.Date(),sep = "")
dir.create(dirname)
#####


#### update at 14:00h ####
source("C:\\Rdir\\Rscripts\\45_vaccine_effect_ICU_compare.R")
source("C:\\Rdir\\Rscripts\\45_vaccine_effect_clinic_compare.R")
source("C:\\Rdir\\Rscripts\\01_NICE_data-week_age_graph.R")
source("C:\\Rdir\\Rscripts\\01_lcps_data_graph_new_small.R")


beds_current_capacity <- beds_current_capacity
source("C:\\Rdir\\Rscripts\\01_lcps_data_zoom.R")
# source("C:\\Rdir\\Rscripts\\68_OMT_counter.R")

###
###

#### update at 15:15 ####

#current.backlog <- 0
#source("C:\\Rdir\\Rscripts\\88_rivm_backlog.R")



tested_daily <- fromJSON(txt = "https://data.rivm.nl/covid-19/COVID-19_uitgevoerde_testen.json")
last(tested_daily$Date_of_report)




source("C:\\Rdir\\Rscripts\\18_daily_pos_rate.R")

source("C:\\Rdir\\Rscripts\\19_Reproduction.R")

source("C:\\Rdir\\Rscripts\\02_RIVM_data_importer_and_archiver.R")

source("C:\\Rdir\\Rscripts\\04_DataPrep.R")

print(Working_Set)

source("C:\\Rdir\\Rscripts\\05_DailyPlots.R")

source("C:\\Rdir\\Rscripts\\06_WoW_DoD_plots.R")

source("C:\\Rdir\\Rscripts\\07_Age-heatmap_barchart.R")

source("C:\\Rdir\\Rscripts\\07A_Age-heatmap_barchart-hosp.R")

source("C:\\Rdir\\Rscripts\\07b_Age-plot-hosp_death.R")

source("C:\\Rdir\\Rscripts\\08b_caseType.R")

source("C:\\Rdir\\Rscripts\\09_WeekOfDeath-plot.R")

source("C:\\Rdir\\Rscripts\\09A_growth.R")

source("C:\\Rdir\\Rscripts\\12_cities.R")

source("C:\\Rdir\\Rscripts\\12_cities_Lansingerland.R")  # Lansingerland

source("C:\\Rdir\\Rscripts\\14_provincies.R")

source("C:\\Rdir\\Rscripts\\20_carehomes.R")

source("C:\\Rdir\\Rscripts\\22_prediction.R")

#source("C:\\Rdir\\Rscripts\\29_coronamelder.R")

source("C:\\Rdir\\Rscripts\\30_weekly_numbers.R")


source("C:\\Rdir\\Rscripts\\44_vaccine_effect_care_compare.R")   #### make new
######
source("C:\\Rdir\\Rscripts\\45_vaccine_effect_ICU_compare.R")
source("C:\\Rdir\\Rscripts\\45_vaccine_effect_clinic_compare.R")

source("C:\\Rdir\\Rscripts\\46_vaccine_effect_age_casus_plot.R")       
source("C:\\Rdir\\Rscripts\\46_vaccine_effect_age_casus_plot_abs.R")
source("C:\\Rdir\\Rscripts\\46_vaccine_effect_age_casus_plot_phd.R")

source("C:\\Rdir\\Rscripts\\47_vaccine_effect_disabled.R")    ### make new000 

#source("C:\\Rdir\\Rscripts\\21_ECDC.R")

source("C:\\Rdir\\Rscripts\\26_Municipality.R")

source("C:\\Rdir\\Rscripts\\01_NICE_data-week_age_graph.R")

source("C:\\Rdir\\Rscripts\\45_vaccine_effect_ICU_compare.R")
source("C:\\Rdir\\Rscripts\\45_vaccine_effect_clinic_compare.R")
source("C:\\Rdir\\Rscripts\\45_vaccine_effect_clinic_compare_young.R")


###
###
###

#### check correct last tweet ####

###
###

source("C:\\Rdir\\Rscripts\\01_lcps_data_new-hosp.R")

source("C:\\Rdir\\Rscripts\\10_TwitterTread.R")



# source("C:\\Rdir\\Rscripts\\05_YearCompare_ALL.R")
# source("C:\\Rdir\\Rscripts\\83_Delta_vs_Omicron_compare.R")

#source("C:\\Rdir\\Rscripts\\82_bierviltje_plus_delta.R")
#source("C:\\Rdir\\Rscripts\\82_bierviltje_plus_Omicron.R")
source("C:\\Rdir\\Rscripts\\82_bierviltje_plus_Omicron_adjusted.R")

 
 