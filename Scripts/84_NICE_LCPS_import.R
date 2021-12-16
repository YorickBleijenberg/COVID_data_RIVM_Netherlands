

####  NICE and LCPS import ####


### NICE bezetting


# IC patients currently
ic_current <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/intake-count/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)
ic_current <- as.data.frame(ic_current)
ic_current$date <- as.Date(ic_current$date)
colnames(ic_current) <- c("date","IC_bezetting_NICE")

## clinic patients currently

zkh_current <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/intake-count/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)
zkh_current <- as.data.frame(zkh_current)
zkh_current$date <- as.Date(zkh_current$date)
colnames(zkh_current) <- c("date","Clinic_bezetting_NICE")

### NICE intake

### IC  ####


# New patients at IC 
ic_intake <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/new-intake/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill=TRUE)

ic_intake <- as.data.frame(t(ic_intake[c(1,2,4),]))

ic_intake$date <- unlist(ic_intake$V1)
ic_intake$ic_intake_proven <- unlist(ic_intake$V2)
ic_intake$ic_intake_suspected <- unlist(ic_intake$V3)
ic_intake <- ic_intake[,c(4:6)]

ic_intake$sum = ic_intake$ic_intake_proven+ic_intake$ic_intake_suspected

intake.total <- ic_intake
intake.total$sum_ic <-intake.total$sum


# New patients at kliniek

json_zkh_df <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/new-intake/",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill=TRUE)

zkh_new <- as.data.frame(t(json_zkh_df[c(1,2,4),]))

zkh_new$date <- unlist(zkh_new$V1)
zkh_new$new_hosp_proven <- unlist(zkh_new$V2)
zkh_new$new_hosp_suspected <- unlist(zkh_new$V3)
zkh_new <- zkh_new[,c(4:6)]

zkh_new$sum = zkh_new$new_hosp_proven+zkh_new$new_hosp_suspected

#combi intake year
intake.total$sum_ic <-intake.total$sum
zkh_new.work <-zkh_new
zkh_new.work$sum_zkh <- zkh_new.work$sum
instroom.combi.year = merge(intake.total,zkh_new.work, by="date")

instroom.combi.year <- instroom.combi.year[,-c(2:4,6:8),]
instroom.combi.year$date <- as.Date(instroom.combi.year$date)


#### LCPS

LCPS_datafeed<-read.csv("https://lcps.nu/wp-content/uploads/covid-19-datafeed.csv",sep=",")  
LCPS_datafeed$date <- as.Date(LCPS_datafeed$Datum ,format="%d-%m-%Y")



LCPS_NICE_combi = merge(instroom.combi.year,LCPS_datafeed, by="date")

LCPS_NICE_combi = merge(LCPS_NICE_combi,ic_current, by="date")
LCPS_NICE_combi = merge(LCPS_NICE_combi,zkh_current, by="date") 


LCPS_NICE_combi <- LCPS_NICE_combi[,-c(4,6),]
LCPS_NICE_combi <- LCPS_NICE_combi[LCPS_NICE_combi$date > "2021-08-31",]


File_date_134 <- paste0("data/",Sys.Date(),"/",Sys.Date(),"_lcps_nice_all.csv")
write.csv2(LCPS_NICE_combi, File_date_134, row.names=FALSE) 

