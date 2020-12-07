library(tidyverse)
library(jsonlite)
require(data.table)
library(lubridate)


#### Behaviour

#repro.name <- "https://data.rivm.nl/covid-19/COVID-19_gedrag.json" #C:\\Rdir\\Mobility\\rivm-data\\2020-10-29_COVID-19_reproductiegetal.json"
#behaviour.raw <- fromJSON(repro.name)

#behaviour.raw  <-  read.csv("https://data.rivm.nl/covid-19/COVID-19_gedrag.csv",sep=";")

behaviour.raw  <-  read.csv("C:\\Rdir\\data\\2020-11-30\\COVID-19_gedrag.csv",sep=";")


#last.date <- tail(behaviour.raw$Date, 1)
#today <- Sys.Date()

# last.date <- format(Sys.time(), "%Y-%m-%d")

#File_date_6 <- paste0("rivm-data/",today,"_COVID-19_gedrag.csv")
#write.csv2(behaviour.raw, File_date_6, row.names=FALSE) 




behaviour.raw.short <- behaviour.raw[-c(5,)]






ggplot(behaviour.raw)
  