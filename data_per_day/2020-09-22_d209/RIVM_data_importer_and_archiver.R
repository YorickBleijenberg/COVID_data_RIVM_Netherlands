#getting today's date, for archiving purposes.
Datum <- format(Sys.time(), "%Y-%m-%d")   

#reading from RIVM website, adding date and weeknumber for easy prossessing down the line.
RIVM_casus_landelijk<-read.csv("https://data.rivm.nl/covid-19/COVID-19_casus_landelijk.csv",sep=";")  #import from RIVM website
RIVM_casus_landelijk$datum<-as.Date(RIVM_casus_landelijk$Date_statistics)   #Adding a date to the case
RIVM_casus_landelijk$week<-strftime(RIVM_casus_landelijk$datum,format = "%V")   #adding week_number to the case

#reading from RIVM website, adding date and weeknumber for easy prossessing down the line.
RIVM_aantallen_gemeente_per_dag<-read.csv("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv",sep=";")  #import from RIVM website
RIVM_aantallen_gemeente_per_dag$datum<-as.Date(RIVM_aantallen_gemeente_per_dag$Date_of_publication)   #Adding a date to the case
RIVM_aantallen_gemeente_per_dag$week<-strftime(RIVM_aantallen_gemeente_per_dag$datum,format = "%V")   #adding week_number to the case

#reading from RIVM website, adding date and weeknumber for easy prossessing down the line.
RIVM_aantallen_gemeente_cumulatief<-read.csv("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv",sep=";")  #import from RIVM website
RIVM_aantallen_gemeente_cumulatief$datum<-as.Date(RIVM_aantallen_gemeente_cumulatief$Date_of_report)   #Adding a date to the case
RIVM_aantallen_gemeente_cumulatief$week<-strftime(RIVM_aantallen_gemeente_cumulatief$datum,format = "%V")   #adding week_number to the case

 

# adding date and writing the file
File_date <- paste0("data/",format(Sys.time(), "%Y-%m-%d"), "_COVID-19_casus_landelijk.csv")  # adding date and writing the file
write.csv2(RIVM_casus_landelijk, File_date) 

# adding date and writing the file
File_date <- paste0("data/",format(Sys.time(), "%Y-%m-%d"), "_COVID-19_aantallen_gemeente_per_dag.csv")  # adding date and writing the file
write.csv2(RIVM_aantallen_gemeente_per_dag, File_date) 

# adding date and writing the file
File_date <- paste0("data/",format(Sys.time(), "%Y-%m-%d"), "_COVID-19_aantallen_gemeente_cumulatief.csv")  # adding date and writing the file
write.csv2(RIVM_aantallen_gemeente_cumulatief, File_date) 




#write.csv(RIVM_casu_landelijk, file = 'data/car-speeds4.csv')#for checking purposes    
#write.csv(RIVM_casu_landelijk, file = paste('carSpeedDate',Datum, '.csv' , sep = '')
#st=format(Sys.time(), "%Y-%m-%d")
#paste("filename_",st, ".pdf", sep = "")
