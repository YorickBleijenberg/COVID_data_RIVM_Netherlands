

#import from LCPS website

LCPS_datafeed<-read.csv("https://lcps.nu/wp-content/uploads/covid-19.csv",sep=",")  
LCPS_datafeed$Datum <- as.Date(LCPS_datafeed$Datum ,format="%d-%m-%Y")
LCPS_datafeed$week<-strftime(LCPS_datafeed$Datum,format = "%V")

File_date_5a <- paste0("data/",format(Sys.time(), "%Y-%m-%d"),"/",format(Sys.time(), "%Y-%m-%d"), "_COVID-19_LCSP.csv")
File_date_5b <- paste0("LCPS-data/","COVID-19_LCSP_", format(Sys.time(), "%Y-%m-%d"),".csv")

write.csv2(LCPS_datafeed, File_date_5a, row.names=FALSE) 
write.csv2(LCPS_datafeed, File_date_5b, row.names=FALSE)


#import from storage

LCPS_datafeed_old<-read.csv("C:\\Rdir\\LCPS-data\\tm10_07.csv",sep=";")  
LCPS_datafeed_old$date <- as.Date(LCPS_datafeed_old$date ,format="%Y-%m-%d")



