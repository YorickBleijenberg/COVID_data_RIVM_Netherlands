library(cbsodataR)
library(sf)
library(dplyr)
library(extrafont)
library(ggplot2)
library(tidyverse)
library(RcppRoll)
require(data.table)

##read the files we need

####copy.aantal.landelijk.gem.dag <- RIVM_casus_landelijk
read.aantal.landelijk.path <- paste("C:\\Rdir\\data\\",Sys.Date(),"\\", Sys.Date(), "_COVID-19_casus_landelijk.csv",sep="")

copy.aantal.landelijk.gem.dag <- read.csv(read.aantal.landelijk.path,sep=";")
AgeFill <- read.csv(read.aantal.landelijk.path,sep=";")

read.aantal.path <-paste("C:\\Rdir\\data\\",Sys.Date(),"\\", Sys.Date(), "_COVID-19_aantallen_gemeente_per_dag.csv", sep = "")
copy.aantal.gem.dag <- read.csv(read.aantal.path,sep=";")




##make a copy so we can use the file later.
copy_cases <- aggregate(copy.aantal.gem.dag$Total_reported, by=list(dateInTable=copy.aantal.gem.dag$date), FUN=sum)
copy_hosp <- aggregate(copy.aantal.gem.dag$Hospital_admission, by=list(Category=copy.aantal.gem.dag$date), FUN=sum)
copy_dead <- aggregate(copy.aantal.gem.dag$Deceased, by=list(Category=copy.aantal.gem.dag$date), FUN=sum)

colnames(copy_cases) <- c("dateInTable", "cases")
colnames(copy_hosp) <- c("dateInTable", "hosp")
colnames(copy_dead) <- c("dateInTable", "dead")
cases_hosp <- merge(copy_cases,copy_hosp, by.x= "dateInTable")
Merged_data <- merge(copy_dead,cases_hosp, by.x= "dateInTable")


Date <- Sys.Date()
Dateyesterday <- Sys.Date()-1
DateAweekAgo <- Sys.Date()-7

Merged_data_7MA <- Merged_data
Working_Set <- subset(Merged_data, dateInTable==Date | dateInTable==DateAweekAgo | dateInTable==Dateyesterday )