#library(cbsodataR)
#library(sf)
#library(dplyr)
#library(extrafont)
#library(ggplot2)
#library(RcppRoll)
#require(data.table)
library(tidyverse)
library(zoo)

##read the files we need

####copy.aantal.landelijk.gem.dag <- RIVM_casus_landelijk
read.aantal.landelijk.path <- paste("C:\\Rdir\\data\\",Sys.Date(),"\\", Sys.Date(), "_COVID-19_casus_landelijk.csv",sep="")

copy.aantal.landelijk.gem.dag <- read.csv(read.aantal.landelijk.path,sep=";")
AgeFill <- read.csv(read.aantal.landelijk.path,sep=";")

######copy.aantal.gem.dag   <-   aantallen_gemeente_per_dag
read.aantal.path <-paste("C:\\Rdir\\data\\",Sys.Date(),"\\", Sys.Date(), "_COVID-19_aantallen_gemeente_per_dag.csv", sep = "")
copy.aantal.gem.dag <- read.csv(read.aantal.path,sep=";")


##filter the cases. hosp. and deceased
copy_cases <- aggregate(copy.aantal.gem.dag$Total_reported,     by=list(dateInTable=copy.aantal.gem.dag$date), FUN=sum)
copy_hosp  <- aggregate(copy.aantal.gem.dag$Hospital_admission, by=list(Category=copy.aantal.gem.dag$date), FUN=sum)
copy_dead  <- aggregate(copy.aantal.gem.dag$Deceased,           by=list(Category=copy.aantal.gem.dag$date), FUN=sum)

colnames(copy_cases) <- c("dateInTable", "cases")
colnames(copy_hosp) <- c("dateInTable", "hosp")
colnames(copy_dead) <- c("dateInTable", "dead")
cases_hosp <- merge(copy_cases,copy_hosp, by.x= "dateInTable")


##### datafile aantallen_gemeente_per_dag
Merged_data <- merge(copy_dead,cases_hosp, by.x= "dateInTable")



Merged_data_2 <- Merged_data
Merged_data_2$MACases  <- rollmeanr(Merged_data_2$cases, 7, fill = 0)
Merged_data_2$MAhosp <- rollmeanr(Merged_data_2$hosp, 7, fill = 0)
Merged_data_2$MAdead <- rollmeanr(Merged_data_2$dead, 7, fill = 0)


Merged_data_2$ma_c_lag  <- lag(Merged_data_2$MACases,7)
Merged_data_2$gf_c <- round((((Merged_data_2$MACases/Merged_data_2$ma_c_lag)-1)*100), digits = 1)
Merged_data_2$ma_h_lag  <- lag(Merged_data_2$MAhosp,7)
Merged_data_2$gf_h <- round((((Merged_data_2$MAhosp/Merged_data_2$ma_h_lag)-1)*100), digits = 1)
Merged_data_2$ma_d_lag  <- lag(Merged_data_2$MAdead,7)
Merged_data_2$gf_d <- round((((Merged_data_2$MAdead/Merged_data_2$ma_d_lag)-1)*100), digits = 1)



Date <- Sys.Date()
Dateyesterday <- Sys.Date()-1
DateAweekAgo <- Sys.Date()-7

Working_Set <- subset(Merged_data_2, dateInTable==Date | dateInTable==DateAweekAgo | dateInTable==Dateyesterday )



