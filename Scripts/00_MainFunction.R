##


#Als ik de regering was, zou ik het ook niet hebben over de doden.
#want: "elke dode is er één teveel".  
#en hier zie je eigenlijk gewoon de kosten van falend beleid in mensenlevens.


#### de-kosten-van-falend-corona-beleid-zijn-mensenlevens.
#### we-zijn-er-nog-lang-niet


####Check LSPC update
LCPS_datafeed<-read.csv("https://lcps.nu/wp-content/uploads/covid-19-datafeed.csv",sep=",")
#LCPS_datafeed<-read.csv("C:\\Rdir\\data\\covid-19_lcpsfeed.csv", sep=",")
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

#source("C:\\Rdir\\Rscripts\\geom_stream.R")
source("C:\\Rdir\\Rscripts\\store\\03A_TwitterAuthentication.r")

#### some constants for the update ####
editionname <-  "Houdt-de-werkelijkheid-zich-een-beetje-aan-de-prognose?"  #"AlLEeN-sAmEn-KrIjGeN-wE-cOrOnA" 
vaccine.edition.name <- "BIZAR:-3de-prikken-niet-in-de-data"
leeftijd             <- "2009"

editienaam  <-  editionname 

#### Start  ####
dirname <- paste("data/",Sys.Date(),sep = "")
dir.create(dirname)
#####


#### update at 14:00h ####
source("C:\\Rdir\\Rscripts\\45_vaccine_effect_ICU_compare.R")
source("C:\\Rdir\\Rscripts\\45_vaccine_effect_clinic_compare.R")
source("C:\\Rdir\\Rscripts\\01_NICE_data-week_age_graph.R")
source("C:\\Rdir\\Rscripts\\01_lcps_data_graph_new_small.R")
source("C:\\Rdir\\Rscripts\\68_OMT_counter.R")

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

source("C:\\Rdir\\Rscripts\\44_vaccine_effect_care_compare.R")

source("C:\\Rdir\\Rscripts\\45_vaccine_effect_ICU_compare.R")
source("C:\\Rdir\\Rscripts\\45_vaccine_effect_clinic_compare.R")

source("C:\\Rdir\\Rscripts\\46_vaccine_effect_age_casus_plot.R")       
source("C:\\Rdir\\Rscripts\\46_vaccine_effect_age_casus_plot_abs.R")
source("C:\\Rdir\\Rscripts\\46_vaccine_effect_age_casus_plot_phd.R")

source("C:\\Rdir\\Rscripts\\47_vaccine_effect_disabled.R")     

source("C:\\Rdir\\Rscripts\\21_ECDC.R")

source("C:\\Rdir\\Rscripts\\26_Municipality.R")

source("C:\\Rdir\\Rscripts\\01_NICE_data-week_age_graph.R")


###
###
###

#### check correct last tweet ####

###
###


source("C:\\Rdir\\Rscripts\\10_TwitterTread.R")




#### tweet.NICE.NEW.tweet ####

tweet.NICE.NEW.tweet <- "Opnames per week - NICE"

tweet.NICE.NEW.tweet <- sprintf(tweet.NICE.NEW.tweet)
Encoding(tweet.NICE.NEW.tweet) <- "UTF-8"

# post_tweet(tweet.NICE.NEW.tweet,  media = c("data/plots/77_NICE_age_hosp_per_week.png", "data/plots/77_NICE_age_hosp_per_week_rel.png",
#                                            "data/plots/77_NICE_age_IC_per_week.png","data/plots/77_NICE_age_IC_per_week_rel.png" 
# ), in_reply_to_status_id = get_reply_id())


 
 
 
 source("C:\\Rdir\\Rscripts\\10_TwitterTread.R")
 
 
 
 
 
 
 
 ###
 *****
 
 ####
 *****
 
 ####
 
 
 
 
 
 # source("C:\\Rdir\\Rscripts\\41_vaccination_dashboard_import.R")
 
 
 
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
 vac.perc.12
 vac.perc.12.second
 
 
 
 
 tweet.percentages.tweet <- "Schattingen
 

 

 
 12+
 - 1e prik: %s%s
 - Volledig gevaccineerd: %s%s
 
 
 Alle Nederlanders
 - 1e prik: %s%s
 - Volledig gevaccineerd: %s%s
 

 - 1e prikken: %s miljoen
 - Volledig gevaccineerd: %s miljoen
 
Niet tegen Delta beschermd:
- %s miljoen
- %s%s
 
"
 
 tweet.percentages.tweet <- sprintf(  tweet.percentages.tweet,
                                      #  vac.perc.18,        deP,
                                      # vac.perc.18.second, deP,
                                      
                                      vac.perc.12,        deP,
                                      vac.perc.12.second, deP,
                                      
                                      vac.perc,           deP,
                                      vac.perc.second,    deP,
                                      
                                      people.vaccinated.short,
                                      people.fully.vaccinated.short,
                                      
                                      people.fully.NOT.vaccinated.short,
                                      not.ful.vac.perc,    deP
 )
 
 Encoding(tweet.percentages.tweet) <- "UTF-8"
#  post_tweet(tweet.percentages.tweet,  media = c("data/belgische_hokjes.png"), in_reply_to_status_id = get_reply_id())
 
 