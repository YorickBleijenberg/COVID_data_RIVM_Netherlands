##



####Check LSPC update
LCPS_datafeed<-read.csv("https://lcps.nu/wp-content/uploads/covid-19.csv",sep=",")
LCPS_datafeed$Datum <- as.Date(LCPS_datafeed$Datum ,format="%d-%m-%Y")
LCPS_datafeed <- LCPS_datafeed[order(LCPS_datafeed$Datum),]
last(LCPS_datafeed,2)
last(LCPS_datafeed$Datum) == Sys.Date()

#library(rsconnect)

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
# moedig-voorwaarts
# Niemand-had-dit-zien-aankomen
# we-hebben-domme-dingen-gedaan-in-het-zicht-van-de-haven
editionname <- "Welkom-terug-in-risiconiveau:-ZEER-ERNSTIG" #  De beste methode om het virus te bestrijden is het niet te hebben.a"  De-R-is-toch-in-de-maand #Reizen-tijdens-klimaat-ramp crisis-in-een-crisis
vaccine.edition.name <- "grote-vaccinatie-update"
leeftijd             <- "2009"

editienaam  <-  editionname 
#number.in.DE <- "0"

#### Start  ####
dirname <- paste("data/",Sys.Date(),sep = "")
dir.create(dirname)
#####


#### update at 14:00h ####
source("C:\\Rdir\\Rscripts\\45_vaccine_effect_ICU_compare.R")
source("C:\\Rdir\\Rscripts\\45_vaccine_effect_clinic_compare.R")
source("C:\\Rdir\\Rscripts\\01_NICE_data-week_age_graph.R")
source("C:\\Rdir\\Rscripts\\01_lcps_data_graph.R")

###
###

#### update at 15:15 ####

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

source("C:\\Rdir\\Rscripts\\08_caseType_diff.R")
   
source("C:\\Rdir\\Rscripts\\08b_caseType.R")

source("C:\\Rdir\\Rscripts\\09_WeekOfDeath-plot.R")

source("C:\\Rdir\\Rscripts\\09A_growth.R")

source("C:\\Rdir\\Rscripts\\12_cities.R")

source("C:\\Rdir\\Rscripts\\12_cities_Lansingerland.R")  # Lansingerland

source("C:\\Rdir\\Rscripts\\14_provincies.R")

source("C:\\Rdir\\Rscripts\\20_carehomes.R")

source("C:\\Rdir\\Rscripts\\22_prediction.R")

source("C:\\Rdir\\Rscripts\\29_coronamelder.R")

source("C:\\Rdir\\Rscripts\\30_weekly_numbers.R")




###
###
###

#### check correct last tweet ####

###
###
###


 ##  source("C:\\Rdir\\Rscripts\\10_EN_TwitterTread.R")


##  source("C:\\Rdir\\Rscripts\\35_vaccinated.R")
##  source("C:\\Rdir\\Rscripts\\43_NICE_vaccine_effect.R")
#   source("C:\\Rdir\\Rscripts\\17_Herstvakantie-effect.R")
#   source("C:\\Rdir\\Rscripts\\33_christmas_deaths.R")


source("C:\\Rdir\\Rscripts\\44_vaccine_effect_care_compare.R")


source("C:\\Rdir\\Rscripts\\45_vaccine_effect_ICU_compare.R")
source("C:\\Rdir\\Rscripts\\45_vaccine_effect_clinic_compare.R")

source("C:\\Rdir\\Rscripts\\46_vaccine_effect_age_casus_plot.R")       
source("C:\\Rdir\\Rscripts\\46_vaccine_effect_age_casus_plot_phd.R")

source("C:\\Rdir\\Rscripts\\47_vaccine_effect_disabled.R")     

source("C:\\Rdir\\Rscripts\\21_ECDC.R")



##
##



source("C:\\Rdir\\Rscripts\\26_Municipality.R")






#source("C:\\Rdir\\Rscripts\\48_vaccine_in_storage.R")     
#source("C:\\Rdir\\Rscripts\\50_vaccine_in_storage_seperate.R")

 
 
 
 
 
 #### tweet.hospital.effect.tweet ####
 
 tweet.hospital.effect.tweet <- "Opnames"
 
 tweet.hospital.effect.tweet <- sprintf(tweet.hospital.effect.tweet)
 Encoding(tweet.hospital.effect.tweet) <- "UTF-8"
 
 post_tweet(tweet.hospital.effect.tweet,  media = c("data/plots/70_vaccinated_compare_age_clinic_abs.png",
                                                    "data/plots/16x_omt_check_nice_peak.png",
                                                    # "data/plots/70_vaccinated_compare_age_clinic.png",
                                                    "data/plots/71_vaccinated_compare_age_ICU_abs.png",
                                                    # "data/plots/71_vaccinated_compare_age_IC.png"
                                                    "data/plots/16x_omt_check_week_on_week.png" 
 ), in_reply_to_status_id = get_reply_id())
 
 
 
 
 source("C:\\Rdir\\Rscripts\\10_TwitterTread.R")
 
 
 
 
 
 
 
 ###
 *****
 
 ####
 *****
 
 ####
 
 
 
 
 
 source("C:\\Rdir\\Rscripts\\41_vaccination_dashboard_import.R")
 
 
 
 vaccins.estimated.total
 estimated.new.today
 people.vaccinated
 people.fully.vaccinated
 
 vac.perc
 vac.perc.second
 
 ggd.new.today
 ha.new.today
 care.new.today
 
 freezer
 in.freezer
 
 long.est
 
 vac.perc.18
 vac.perc.18.second
 
 
 #vac.perc.second <- 48.5
 
 
 
 tweet.percentages.tweet <- "Schattingen
 

 

 
 12+
 - 1e prik: %s%s
 - Volledig gevaccineerd: %s%s
 
 
 Alle Nederlanders
 - 1e prik: %s%s
 - Volledig gevaccineerd: %s%s
 

 - 1e prikken: %s miljoen
 - Volledig gevaccineerd: %s miljoen
 
"
 
 tweet.percentages.tweet <- sprintf(  tweet.percentages.tweet,
                                      #  vac.perc.18,        deP,
                                      # vac.perc.18.second, deP,
                                      
                                      vac.perc.12,        deP,
                                      vac.perc.12.second, deP,
                                      
                                      vac.perc,           deP,
                                      vac.perc.second,    deP,
                                      
                                      people.vaccinated.short,
                                      people.fully.vaccinated.short
 )
 
 Encoding(tweet.percentages.tweet) <- "UTF-8"
 post_tweet(tweet.percentages.tweet,  media = c("data/belgische_hokjes.png"), in_reply_to_status_id = get_reply_id())
 
 
 #  Volwassenen
 # - 1e prik: %s%s
 # - Volledig gevaccineerd: %s%s