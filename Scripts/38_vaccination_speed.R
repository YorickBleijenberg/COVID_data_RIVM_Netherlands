

### vaccination speed.  
###dosis given per day
####  target speed ###

#live tracker vs planning

### aantal kwetsbare ouderen dat nu beschermend had kunnen zijn  #### 

### 2020-12-26 ---  https://www.omroepbrabant.nl/nieuws/3315778/eerste-coronavaccins-aangekomen-bij-movianto-in-oss-historisch-moment


####    Comirnaty (Pfizer/BioNTech)

###  predection when you get a shot.








#vaccins.dailydata <- data.frame(as.Date(Sys.Date()),vaccins_totaal_toegediend,vaccins.toegediend.totaal_geschat,vaccins.geleverd.totaal,vaccins.ggd,vaccins.ziekenhuizen,vaccins.zorginstellingen.geschat) ## Calculate totals for cases, hospitalizations, deaths
#names(vaccins.dailydata) <- c("date","vaccines_administered","vaccines_administered_estimated","vaccines_expected_6weeks","vaccines_administered_ggd","vaccines_administered_estimated_hospital","vaccines_administered_estimated_carehomes")

#filename.daily.vaccins <- paste0("data-rivm/vaccines-per-day/rivm_daily_vaccines_",Sys.Date(),".csv")

#write.csv(vaccins.dailydata, file = filename.daily.vaccins, row.names = F)

# 
#temp = list.files(path = "data-rivm/vaccines-per-day/",pattern="*.csv", full.names = T) ## Fetch all day files
#myfiles = lapply(temp, read.csv) ## Load all day files

#vaccines_by_day <- map_dfr(myfiles, ~{ ## Write dataframe of all day files
#  .x
#})

#vaccines_by_day$date <- as.Date(vaccines_by_day$date)
#vaccines_by_day <- vaccines_by_day[order(vaccines_by_day$date),]

#write.csv(vaccines_by_day, file = "data/vaccines_by_day.csv",row.names = F) ## Write file with aggregate data per day



#### Vaccines delivered in the future.
#dat <- fromJSON(txt = "https://coronadashboard.rijksoverheid.nl/json/NL.json")
#vaccines_delivery <- as.data.frame(dat$vaccine_delivery$values)
#vaccines_delivery$date <- as.Date(as.POSIXct(vaccines_delivery$date_of_insertion_unix, origin="1970-01-01"))
#filename.daily.vaccins.delivered <- paste0("data-rivm/vaccines-delivered/rivm_daily_vaccines_",Sys.Date(),".csv")
#write.csv(vaccines_delivery, file = filename.daily.vaccins.delivered, row.names = F)
#sum(vaccines_delivery[,c("astra_zeneca","pfizer","cure_vac","janssen","moderna","sanofi")])

