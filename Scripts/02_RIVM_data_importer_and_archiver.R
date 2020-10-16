#require(tidyverse)
#require(rjson)
#require(data.table)

rm(list=ls())

CBS_age_10yrs_GH <-read.csv("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/data-cbs/CBS_age_10yr_groups.csv",sep=";")

#reading from RIVM website, adding date and weeknumber for easy prossessing down the line.
RIVM_casus_landelijk<-read.csv("https://data.rivm.nl/covid-19/COVID-19_casus_landelijk.csv",sep=";")  #import from RIVM website
RIVM_casus_landelijk$date<-as.Date(RIVM_casus_landelijk$Date_statistics)   #Adding a date to the case
RIVM_casus_landelijk$week<-strftime(RIVM_casus_landelijk$date,format = "%V")   #adding week_number to the case

#reading from RIVM website, adding date and weeknumber for easy prossessing down the line.
RIVM_aantallen_gemeente_per_dag<-read.csv("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv",sep=";")  #import from RIVM website
RIVM_aantallen_gemeente_per_dag$date<-as.Date(RIVM_aantallen_gemeente_per_dag$Date_of_publication)   #Adding a date to the case
RIVM_aantallen_gemeente_per_dag$week<-strftime(RIVM_aantallen_gemeente_per_dag$date,format = "%V")   #adding week_number to the case

#reading from RIVM website, adding date and weeknumber for easy prossessing down the line.
RIVM_aantallen_gemeente_cumulatief<-read.csv("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv",sep=";")  #import from RIVM website
RIVM_aantallen_gemeente_cumulatief$date<-as.Date(RIVM_aantallen_gemeente_cumulatief$Date_of_report)   #Adding a date to the case
RIVM_aantallen_gemeente_cumulatief$week<-strftime(RIVM_aantallen_gemeente_cumulatief$date,format = "%V")   #adding week_number to the case

RIVM_rioolwaterdata <-read.csv("https://data.rivm.nl/covid-19/COVID-19_rioolwaterdata.csv",sep=",")

dirname <- paste("data/",Sys.Date(),sep = "")
dir.create(dirname)

# adding date to the file names, and adding to the data folder
File_date_1a <- paste0("data/",format(Sys.time(), "%Y-%m-%d"),"/",format(Sys.time(), "%Y-%m-%d"), "_COVID-19_casus_landelijk.csv")
File_date_1b <- paste0("rivm-data/","COVID-19_casus_landelijk_", format(Sys.time(), "%Y-%m-%d"),".csv")
File_date_2 <- paste0("data/",format(Sys.time(), "%Y-%m-%d"),"/",format(Sys.time(), "%Y-%m-%d"), "_COVID-19_aantallen_gemeente_per_dag.csv")  
File_date_3 <- paste0("data/",format(Sys.time(), "%Y-%m-%d"),"/",format(Sys.time(), "%Y-%m-%d"), "_COVID-19_aantallen_gemeente_cumulatief.csv")
File_date_4 <- paste0("data/",format(Sys.time(), "%Y-%m-%d"),"/",format(Sys.time(), "%Y-%m-%d"), "_COVID-19_RIVM_rioolwaterdata.csv")


#writing the files
write.csv2(RIVM_casus_landelijk, File_date_1a, row.names=FALSE) 
write.csv2(RIVM_casus_landelijk, File_date_1b, row.names=FALSE)
write.csv2(RIVM_aantallen_gemeente_per_dag, File_date_2, row.names=FALSE) 
write.csv2(RIVM_aantallen_gemeente_cumulatief, File_date_3, row.names=FALSE)
write.csv2(RIVM_rioolwaterdata, File_date_4, row.names=FALSE)

