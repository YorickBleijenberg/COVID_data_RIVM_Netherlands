#### set dates ####
today = Sys.Date()
yesterday <- today-1

#### check number of doses in freezer (estimation for this week)  ####
opdeplank <-read.csv("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/vaccination/people.vaccinated%20-%20doses.received.csv",sep=",")
opdeplank[is.na(opdeplank)] <- 0
opdeplank$day <- as.Date(opdeplank$day)
opdeplank <- (opdeplank %>% filter(day == today ))
freezer = opdeplank$total_all

#### import historic vaccination data from GH - NOT from the current .json file #####
vacc_date_hist.file <- paste0("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/vaccination/daily-dashboard-update/",yesterday,"_vaccine-data.csv")
vacc_date_hist <-read.csv(vacc_date_hist.file,sep=",")
vacc_date_hist$date_of_update <- as.Date(vacc_date_hist$date_of_update)
vacc_date_hist$date <- as.Date(vacc_date_hist$date)
last(vacc_date_hist$total_estimated)

#### import current vaccination data ####

locale_json <- "https://coronadashboard.rijksoverheid.nl/json/NL.json"
locale_dat <- fromJSON(txt = locale_json)
# check new date and values #
as.Date(as.POSIXct(locale_dat$vaccine_administered_total$last_value$date_unix , origin="1970-01-01"))
locale_dat$vaccine_administered_total$last_value$estimated
### store raw value
long.est <- locale_dat$vaccine_administered_total$last_value$estimated

#### store reported and estimated given. ####
estimated.given.doc <- locale_dat$vaccine_administered_doctors$values
colnames(estimated.given.doc) <- c("date","doctors" ,"date2")
estimated.given.ggd <- locale_dat$vaccine_administered_ggd_ghor$values
colnames(estimated.given.ggd) <- c("date","ggd" ,"date2")
estimated.given.ggd2 <- locale_dat$vaccine_administered_ggd$values
colnames(estimated.given.ggd2) <- c("date","ggd2" ,"date2")
estimated.given.care_hosp <- locale_dat$vaccine_administered_hospitals_and_care_institutions$values
colnames(estimated.given.care_hosp) <- c("date","hospital_care" ,"date2")

estimated.given.all <- merge(estimated.given.ggd,estimated.given.care_hosp, all = TRUE)
estimated.given.all <- merge(estimated.given.all,estimated.given.doc, all = TRUE)
estimated.given.all <- merge(estimated.given.all,estimated.given.ggd2,all = TRUE)

estimated.given.all$date <- as.Date(as.POSIXct(estimated.given.all$date , origin="1970-01-01"))
estimated.given.all$date2 <- as.Date(as.POSIXct(estimated.given.all$date2 , origin="1970-01-01"))

vac.given.filename <- paste0("data/",format(Sys.time(), "%Y-%m-%d"),"/",format(Sys.time(), "%Y-%m-%d"), "_vac.all.given.jsn.csv")
write.csv2(estimated.given.all, file = vac.given.filename, row.names = F)

#### store doses delivered, and still in stock ((not)available) ####
vaccine_stock <- locale_dat$vaccine_stock$values
vaccine_stock$date_unix <- as.Date(as.POSIXct(vaccine_stock$date_unix , origin="1970-01-01"))
vaccine_stock$date_of_insertion_unix <- as.Date(as.POSIXct(vaccine_stock$date_of_insertion_unix , origin="1970-01-01"))
vaccine_stock[is.na(vaccine_stock)] <- 0

vaccine_stock.filename <- paste0("data/",format(Sys.time(), "%Y-%m-%d"),"/",format(Sys.time(), "%Y-%m-%d"), "_vaccine_stock.csv")
write.csv2(vaccine_stock, file = vaccine_stock.filename, row.names = F)

#### store doses delivered, and expected ####
vaccine_delivery_per_supplier <- locale_dat$vaccine_delivery_per_supplier$values

vaccine_delivery_per_supplier$date_of_insertion_unix <- as.Date(as.POSIXct(vaccine_delivery_per_supplier$date_of_insertion_unix , origin="1970-01-01"))
vaccine_delivery_per_supplier$date_of_report_unix <- as.Date(as.POSIXct(vaccine_delivery_per_supplier$date_of_report_unix , origin="1970-01-01"))
vaccine_delivery_per_supplier$date_start_unix <- as.Date(as.POSIXct(vaccine_delivery_per_supplier$date_start_unix , origin="1970-01-01"))
vaccine_delivery_per_supplier$date_end_unix  <- as.Date(as.POSIXct(vaccine_delivery_per_supplier$date_end_unix , origin="1970-01-01"))
vaccine_delivery_per_supplier <- vaccine_delivery_per_supplier[order(vaccine_delivery_per_supplier$date_start_unix ),]   #order by date

vaccine_stock.filename <- paste0("data/",format(Sys.time(), "%Y-%m-%d"),"/",format(Sys.time(), "%Y-%m-%d"), "_vaccine_delivery_per_supplier.csv")
write.csv2(vaccine_delivery_per_supplier, file = vaccine_stock.filename, row.names = F)

# vaccine_delivery<- locale_dat$vaccine_delivery$values

####  Vaccines given - get values for the new day and store ####

vaccins.estimated.total <- as.integer(locale_dat$vaccine_administered_total$last_value$estimated)
vaccins.reported.total  <- as.integer(locale_dat$vaccine_administered_total$last_value$reported)
vaccins.registred.ggd   <- as.integer(locale_dat$vaccine_administered_ggd_ghor$last_value$reported)
vaccins.estimated.ha    <- as.integer(locale_dat$vaccine_administered_doctors$last_value$estimated)
vaccins.estimated.care  <- as.integer(locale_dat$vaccine_administered_hospitals_and_care_institutions$last_value$estimated)
vaccins.registerd.hops  <- as.integer(0)   #is now "hugo schatting"
#vaccins.registerd.hops  <- as.integer(locale_dat$vaccine_administered_lnaz$last_value$reported)
#vaccins.estimated.care  <- as.integer(locale_dat$vaccine_administered_care_institutions$last_value$estimated)


#second dose estimation
second.dose <- as.integer((vaccins.estimated.total/100*22.3)) 

people.vaccinated <- (vaccins.estimated.total-second.dose)  # number of people with at least one dose.

janssen.given <- 38000
people.fully.vaccinated <- second.dose + janssen.given      # number with 2 doses OR 1x Janssen

estimated.new.today <- (vaccins.estimated.total - last(vacc_date_hist$total_estimated))
reported.new.today <- (vaccins.reported.total - last(vacc_date_hist$total_registerd))
full.new.today <- (people.fully.vaccinated - last(vacc_date_hist$people_fully_vaccinated ))  
ggd.new.today <- (vaccins.registred.ggd - last(vacc_date_hist$ggd_total))
hosp.new.today <-  0  # (vaccins.registerd.hops - last(vacc_date_hist$hosp_total ))
care.new.today <- (vaccins.estimated.care - last(vacc_date_hist$care_total))
ha.new.today <- (vaccins.estimated.ha - last(vacc_date_hist$ha_total))

week <- isoweek(Sys.Date()-1)
new.row.df <-0
new.row.df <- data.frame(today, yesterday,week,vaccins.reported.total,
                         vaccins.estimated.total,  people.vaccinated,  people.fully.vaccinated,
                         vaccins.registred.ggd,vaccins.registerd.hops, vaccins.estimated.care,vaccins.estimated.ha,
                         reported.new.today,estimated.new.today,full.new.today,ggd.new.today,hosp.new.today,care.new.today,ha.new.today)

colnames(new.row.df) <- c("date_of_update","date","week","total_registerd",
                          "total_estimated",  "people_vaccinated",  "people_fully_vaccinated",
                          "ggd_total","hosp_total","care_total","ha_total",
                          "registerd_new","estimated_new","people_fully_vaccinated_new","ggd_new" ,"hosp_new","care_new","ha_new")

new.vacc.df <- rbind(vacc_date_hist, new.row.df)

new.vacc.df[is.na(new.vacc.df)] <- 0

daily.vaccination.data.filename.x <- paste0("data/plots/",format(Sys.Date(), "%Y-%m-%d"), "_vaccine-data.csv")
write.csv(new.vacc.df, file = daily.vaccination.data.filename.x, row.names = F)

##### check if max (dagrecord) ######

emoji_syringe <- intToUtf8(0x1F489)
emoji_partyface <- intToUtf8(0x1F973)
#new.vacc.df.short <- new.vacc.df %>% filter(date > "2021-01-20")
estimated.new.today.np <- last(new.vacc.df$estimated_new)

maxValueVac <- max(new.vacc.df$estimated_new, na.rm = TRUE)
dagRecordVaccination <- ""

if(estimated.new.today.np == maxValueVac){
  dagRecordVaccination <- paste("(", intToUtf8(0x1F973),"dagrecord",intToUtf8(0x1F973),"),", sep = "")
}else {
  dagRecordVaccination <- ""}


#### run two other scripts ####

source("C:\\Rdir\\Rscripts\\48_vaccine_in_storage.R")     
source("C:\\Rdir\\Rscripts\\50_vaccine_in_storage_seperate.R")


####### get data t plot ####

new.vacc.df.work <- new.vacc.df

new.vacc.df.work[is.na(new.vacc.df.work)] <- 0

new.vacc.df.work$for_MA <- new.vacc.df.work$estimated_new         # $care_new  + new.vacc.df.work$ggd_new + new.vacc.df.work$ha_new
new.vacc.df.work$MAnew  <- rollmeanr(new.vacc.df.work$for_MA, 7, fill = 0)
new.vacc.df.work$MAnew_lead  <- lead(  new.vacc.df.work$MAnew,3)
s_dayMA_tot <- last(new.vacc.df.work$MAnew)
s_dayMA_tot <- format(as.integer(s_dayMA_tot) ,big.mark = ".", decimal.mark = ",")

#yesterday_new_ggd <- last(people.vaccinated.ggd.gh$new)
#yesterday_new_ggd <- format(yesterday_new_ggd, big.mark="." ,decimal.mark=",")
#subtitle_text_new <- paste("Gisteren gevaccineerd:", yesterday_new_ggd,"\nWe moeten naar 100.000+ per dag (1 miljoen per week)")
#####


key <- "date"
value <- "vacc_total"
#gathercols <- c("Besmettingen","Opnames","Overleden")
gathercols <- c("hosp_new","care_new","ggd_new", "ha_new")
new.vacc.df.work.long <- gather(new.vacc.df.work, key, value, gathercols)

#new.vacc.df.work.long$key <- as.factor(new.vacc.df.work.long$key, c("ggd_new","hosp_new","care_new", "ha_new"))




#######   plot new per day ######


ggplot(new.vacc.df.work.long)+
  geom_col(aes(x=date, y=value, fill =  factor(key, levels=c("care_new", "ha_new","ggd_new","hosp_new")) ), color= "black")+
  
  geom_line( aes(x=date, y=MAnew_lead), color = "#f5f5f5", size = 4)+
  geom_line( aes(x=date, y=MAnew_lead), color = "black", size = 2)+  
  
  scale_x_date(date_breaks = "1 weeks", 
               date_labels= format("%d/%m"),
               limits = as.Date(c("2021-01-6", NA)))+
  scale_y_continuous(limits = c(NA, NA), labels = label_comma(big.mark = ".", decimal.mark = ","))+ 
  
  #coord_cartesian(expand = FALSE)+
  theme_classic()+
  
  xlab("")+
  ylab("")+
  
  scale_fill_manual( values=c("#d7191c", "#2c7bb6",  "#fdae61","#8a9296"), labels=c ("ziekenhuizen/zorginstellingen","huisartsen", "GGD'en","Hugo correctie"))+
  
  # scale_fill_brewer(palette = "RdYlBu", labels=c("zorginstellingen", "GGD'en","huisartsen", "ziekenhuizen" ))+
  
  labs(title = "Vaccinaties per dag",
       # subtitle = subtitle_text_new,
       caption = paste("Bron: github.com/YorickBleijenberg | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(#legend.position = "none",   # no legend
    legend.title = element_blank(),  ## legend title
    legend.position="top",
    legend.background = element_rect(fill="#f5f5f5", size=0.5, linetype="solid"),
    legend.text = element_text(colour="black", size=20, face="bold"))+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title = element_text(hjust = 0.5,size = 40,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
  ggsave("data/plots/94_vaccinated_new.png",width=16, height = 9)  

#key <- "week"
#value <- "vacc_total"
#gathercols <- c("Besmettingen","Opnames","Overleden")
#gathercols <- c("hosp_new","care_new","ggd_new", "ha_new")
#to_play_week_df.long <- gather(to_play_witch_df, key, value, all_of(gathercols))


new.vacc.df.work$week_day <- weekdays(new.vacc.df.work$date)

new.vacc.df.work$week_day <- as.factor(new.vacc.df.work$week_day)

#new.vacc.df.work <- new.vacc.df.work[new.vacc.df.work$date > "2020-06-28",]

new.vacc.df.work$weekbegin <- floor_date(new.vacc.df.work$date, " week", week_start = 1)

this.week <-floor_date(as.Date(today), " week", week_start = 1) 

new.vacc.df.work$new <- new.vacc.df.work$ggd_new+new.vacc.df.work$hosp_new+ new.vacc.df.work$care_new + new.vacc.df.work$ha_new


key <- "week"
value <- "vacc_total"
gathercols <- c("hosp_new","care_new","ggd_new", "ha_new")
new.vacc.df.work.long <- gather(new.vacc.df.work, key, value, gathercols)


library(viridis)



ggplot(new.vacc.df.work, aes(x=weekbegin, y=new , fill = factor(week_day, levels=c("Sunday","Saturday","Friday","Thursday", "Wednesday", "Tuesday","Monday"))))+
  
  geom_bar(stat='identity', color="black")+
  
  scale_y_continuous(limits = c(NA, NA), labels = label_comma(big.mark = ".", decimal.mark = ","))+  #breaks = c(2500, 5000, 7500,10000,12500,15000),
  
  scale_fill_viridis_d() +
  
  #coord_cartesian(expand = FALSE)+
  theme_classic()+
  xlab("")+
  ylab("")+
  
  labs(title = "Vaccinaties per week",
       # subtitle = subtitle_text_new,
       caption = paste("Bron: github.com/YorickBleijenberg | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(legend.position = c(0.05, 0.5),
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    #legend.position = "none",   # no legend
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5,size = 40,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
  ggsave("data/plots/95_vaccinated_week_day.png",width=16, height = 9)  



ggplot(new.vacc.df.work, aes(x=weekbegin, y=new , fill = factor(week_day, levels=c("Sunday","Saturday","Friday","Thursday", "Wednesday", "Tuesday","Monday"))))+
  
  annotate("rect", xmin = as.Date("2021-03-27"), xmax =as.Date("2021-04-02"), ymin =433394, ymax = 513016, color = "black",fill = "darkred", alpha = 0.2)+
  annotate("rect", xmin = as.Date("2021-03-27"), xmax =as.Date("2021-04-02"), ymin =0, ymax = 433394, color = "black",fill = "darkgray", alpha = 0.4)+
  
  
  annotate("rect", xmin = as.Date("2021-04-03"), xmax =as.Date("2021-04-09"), ymin =499362, ymax = 624361, color = "black",fill = "darkred", alpha = 0.2)+
  annotate("rect", xmin = as.Date("2021-04-03"), xmax =as.Date("2021-04-09"), ymin =0, ymax = 499362, color = "black",fill = "gray", alpha = 0.4)+
  annotate("rect", xmin = as.Date("2021-04-03"), xmax =as.Date("2021-04-09"), ymin =624361, ymax = 652559, color = "black",fill = "green", alpha = 0.6)+
  
  annotate("rect", xmin = as.Date("2021-04-10"), xmax =as.Date("2021-04-16"), ymin =697408, ymax = 710263, color = "black",fill = "darkred", alpha = 0.2)+
  annotate("rect", xmin = as.Date("2021-04-10"), xmax =as.Date("2021-04-16"), ymin =0, ymax = 697408, color = "black",fill = "gray", alpha = 0.4)+
  annotate("rect", xmin = as.Date("2021-04-10"), xmax =as.Date("2021-04-16"), ymin =710263, ymax = 757719, color = "black",fill = "green", alpha = 0.6)+
  
  annotate("rect", xmin = as.Date("2021-04-17"), xmax =as.Date("2021-04-23"), ymin =665567, ymax = 691274, color = "black",fill = "darkred", alpha = 0.2)+
  annotate("rect", xmin = as.Date("2021-04-17"), xmax =as.Date("2021-04-23"), ymin =0, ymax = 665567, color = "black",fill = "gray", alpha = 0.4)+
  annotate("rect", xmin = as.Date("2021-04-17"), xmax =as.Date("2021-04-23"), ymin =691274, ymax = 757719, color = "black",fill = "green", alpha = 0.6)+
  
  annotate("rect", xmin = as.Date("2021-04-24"), xmax =as.Date("2021-04-30"), ymin =0, ymax = 668988, color = "black",fill = "gray", alpha = 0.4)+
  annotate("rect", xmin = as.Date("2021-04-24"), xmax =as.Date("2021-04-30"), ymin =668988, ymax = 719219, color = "black",fill = "gray", alpha = 0.6)+
  annotate("rect", xmin = as.Date("2021-04-24"), xmax =as.Date("2021-04-30"), ymin =649762, ymax = 719219, color = "black",fill = "red", alpha = 0.6)+
  
  annotate("rect", xmin = as.Date("2021-05-01"), xmax =as.Date("2021-05-07"), ymin =0, ymax = 826926, color = "black",fill = "gray", alpha = 0.6)+
  annotate("rect", xmin = as.Date("2021-05-01"), xmax =as.Date("2021-05-07"), ymin =826926, ymax = 810012, color = "black",fill = "red", alpha = 0.6)+
  
  annotate("rect", xmin = as.Date("2021-05-08"), xmax =as.Date("2021-05-14"), ymin =0, ymax = 936739, color = "black",fill = "gray", alpha = 0.6)+
  
  annotate("rect", xmin = as.Date("2021-05-15"), xmax =as.Date("2021-05-21"), ymin =0, ymax = 1045985, color = "black",fill = "gray", alpha = 0.6)+
  
  
  geom_bar(stat='identity', color="black")+
  
  scale_y_continuous(limits = c(0, NA), #breaks = c(200000, 400000, 600000),
                     labels = label_comma(big.mark = ".", decimal.mark = ","))+  
  
  scale_fill_viridis_d() +
  
  #coord_cartesian(expand = FALSE)+
  theme_classic()+
  xlab("")+
  ylab("")+
  
  labs(title = "Vaccinaties per week",
       subtitle = "rood = bijstelling verwachting op dashboard",
       caption = paste("Bron: github.com/YorickBleijenberg | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(legend.position = c(0.05, 0.5),
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    #legend.position = "none",   # no legend
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5,size = 40,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
  ggsave("data/plots/95_vaccinated_week_day_estimation.png",width=16, height = 9)  





#####   type graph

ggplot(new.vacc.df.work.long , aes(x=weekbegin, y=value , fill =  factor(key, levels=c("care_new", "ha_new","ggd_new","hosp_new")) ))+
  
  geom_bar(stat='identity', color="black")+
  geom_bar(stat='identity')+
  
  #scale_x_date(date_breaks = "1 weeks", 
  #             date_labels= format("%d/%m"),
  #             limits = as.Date(c("2021-01-6", "2021-02-08")))+
  
  scale_y_continuous(limits = c(NA, NA), labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  scale_fill_manual( values=c("#d7191c", "#2c7bb6",  "#fdae61","#8a9296"), labels=c ("ziekenhuizen/zorginstellingen","huisartsen", "GGD'en","Hugo correctie"))+
  
  #  scale_fill_manual( values=c("#2c7bb6","#d7191c", "#abd9e9",  "#fdae61"), labels=c ("ziekenhuizen","zorginstellingen","huisartsen", "GGD'en" ))+
  #scale_fill_brewer(palette = "RdYlBu", labels=c("zorginstellingen", "GGD'en","huisartsen", "ziekenhuizen" ))+
  
  #coord_cartesian(expand = FALSE)+
  theme_classic()+
  xlab("")+
  ylab("")+
  
  labs(title = "Vaccinaties per week",
       # subtitle = subtitle_text_new,
       caption = paste("Bron: github.com/YorickBleijenberg | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(#legend.position = "none",   # no legend
    legend.title = element_blank(),  ## legend title
    legend.position="top",
    legend.background = element_rect(fill="#f5f5f5", size=0.5, linetype="solid"),
    legend.text = element_text(colour="black", size=20, face="bold")
  )+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    #legend.position = "none",   # no legend
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5,size = 40,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
  ggsave("data/plots/95_vaccinated_week_new.png",width=16, height = 9)  












#  agecbs.df <-read.csv("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/data-cbs/people.vaccinated%20-%20age-reverse.csv",sep=",")




#ggplot(new.vacc.df.work)+
#  geom_col(aes(x= date, y=total_estimated))+

#  geom_hline(data=agecbs.df, mapping=aes(yintercept=aantal), color="black")+
#  geom_text(data=agecbs.df, mapping=aes(x=as.Date("2021-01-01"), y=aantal, label=Leeftijd), size=4, vjust=-0.4, hjust=0)+

# scale_y_continuous(limits = c(0, 18000000),breaks = c(5000000,10000000,15000000), labels = label_comma(big.mark = ".", decimal.mark = ","))+

#  ggsave("data/92_vaccine_age_full.png",width=5, height = 25)


# ggplot(new.vacc.df.work)+
#  geom_col(aes(x= date, y=total_registerd))+
#  geom_col(aes(x= date, y=total_estimated))+

#  geom_hline(data=agecbs.df, mapping=aes(yintercept=aantal), color="black")+
# geom_text(data=agecbs.df, mapping=aes(x=as.Date("2021-01-01"), y=aantal, label=Leeftijd), size=4, vjust=-0.4, hjust=0)+

#  scale_y_continuous(limits = c(0, 2500000),breaks = c(5000000,10000000,15000000), labels = label_comma(big.mark = ".", decimal.mark = ","))+

# ggsave("data/92_vaccine_age_small.png",width=16, height = 9)





start.vaccination.nl = as.Date(c("2021-01-06"))
days.vaccination.in.nl = as.numeric(Sys.Date() - start.vaccination.nl+1)

vaccinated.people.total = vaccins.estimated.total
vaccinated.first = vaccinated.people.total-vaccinated.second

vac.perc <-  round((vaccinated.first/17474693*100), digits =2)
vac.perc <- format(vac.perc, scientific=F)
vac.perc.18 <-  round((vaccinated.first/14070340*100), digits =2)
vac.perc.18 <- format(vac.perc.18, scientific=F)

vac.perc.second <-  round((vaccinated.second/17474693*100), digits =2)
vac.perc.second <- format(vac.perc.second, scientific=F)
vac.perc.18.second <-  round((vaccinated.second/14070340*100), digits =2)
vac.perc.18.second <- format(vac.perc.18.second, scientific=F)


spillage <- as.integer(vaccinated.people.total/99)*1
bescas  <- 174330+70700
in.freezer <- freezer-vaccinated.people.total-spillage-bescas





vaccins.estimated.total <- format(vaccins.estimated.total,big.mark = ".", decimal.mark = ",")
vaccins.reported.total <- format(vaccins.reported.total ,big.mark = ".", decimal.mark = ",")

estimated.new.today <- format(estimated.new.today,big.mark = ".", decimal.mark = ",")
reported.new.today <- format(reported.new.today ,big.mark = ".", decimal.mark = ",")

people.vaccinated <- format(people.vaccinated,big.mark = ".", decimal.mark = ",")
second.dose <- format(second.dose ,big.mark = ".", decimal.mark = ",")


# jaartal = 1961
# leeftijd = 2021-jaartal
# leeftijd2= leeftijd-1


#vaccins.estimated.total
#vaccins.reported.total
#estimated.new.today

#  require(magick);
#  require(stringr);

#  banner.text <- paste(" ",vaccins.estimated.total,"\n\n", vaccins.reported.total)

# banner <- image_read('https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/background/background.png') %>%

#  image_annotate(banner.text , font = 'Cabin', weight = '600', size = 60,  location = "+0+25", gravity = "center", color = "white")#%>%
#  image_annotate( vaccins.reported.total, font = 'Cabin', weight = '600', size = 40,  location = "+0+80", gravity = "center", color = "white") %>%
# image_write(banner, str_c("data/",Sys.Date(),"-banner.png") )




##### set edition name  ###


#### tweet.vaccination.start.tweet ####

tweet.vaccination.start.tweet <- "%sVaccinatiedag %s, De %s editie.

Aantal gisteren gezet:
+%s %s

Totaal (geschat):
%s 

7-daags gemiddelde: +%s per dag.

Aan de beurt: geboren in of vóór 1955 of in 1961 & 1962
coronabeeld.nl"

# Schattingen:
# 1e prik: %s
# 2e prik: %s

#Leeftijd aan de beurt: %s (%s/%s en ouder)

tweet.vaccination.start.tweet <- sprintf(tweet.vaccination.start.tweet,
                                         emoji_syringe, days.vaccination.in.nl, vaccine.edition.name,#emoji_syringe,
                                         estimated.new.today,dagRecordVaccination,
                                         vaccins.estimated.total,
                                         #vaccins.reported.total,reported.new.today,
                                         s_dayMA_tot #,
                                       #  jaartal, leeftijd,leeftijd2
                                         
)

Encoding(tweet.vaccination.start.tweet) <- "UTF-8"

baner.path<- paste0("data/",Sys.Date(),"-banner.png")

post_tweet(tweet.vaccination.start.tweet,   media = c("data/plots/94_vaccinated_new.png",
                                                      "data/plots/98_leeftijd_relatief_care.png",
                                                      "data/plots/95_vaccinated_week_day.png",
                                                      "data/plots/80_vaccine_on_shelf.png")) # media = c(baner.path))





ggd.new.today  <-  format(ggd.new.today ,big.mark = ".", decimal.mark = ",")
hosp.new.today <-  format(hosp.new.today ,big.mark = ".", decimal.mark = ",")
care.new.today <-  format(care.new.today ,big.mark = ".", decimal.mark = ",")
ha.new.today   <-  format(ha.new.today ,big.mark = ".", decimal.mark = ",")


#### tweet.vaccination.speed.tweet ####

tweet.vaccination.speed.tweet <- "Aantal prikken gisteren:
- GGD'en: +%s
- Huisartsen: +%s    (schatting)
- Ziekenhuizen & Zorginstellingen: +%s  (schatting)
"
tweet.vaccination.speed.tweet <- sprintf(tweet.vaccination.speed.tweet,
                                         ggd.new.today,
                                         #hosp.new.today,
                                         ha.new.today,
                                         care.new.today
                                         
)
Encoding(tweet.vaccination.speed.tweet) <- "UTF-8"
post_tweet(tweet.vaccination.speed.tweet,  media = c("data/plots/94_vaccinated_new.png"), in_reply_to_status_id = get_reply_id())


#### tweet.vaccination.week.tweet ####

tweet.vaccination.week.tweet <- "Vaccinaties per:
- Dag van de week
- Organisatie, per week
Doden
- naar week van overlijden (max 2de golf op 100%s)
- Verpleeghuizen vs de rest"
tweet.vaccination.week.tweet <- sprintf(tweet.vaccination.week.tweet,
                                        deP)
Encoding(tweet.vaccination.week.tweet) <- "UTF-8"
post_tweet(tweet.vaccination.week.tweet,  media = c( "data/plots/95_vaccinated_week_new.png", 
                                                     "data/plots/95_vaccinated_week_day_estimation.png",
                                                     "data/plots/74_dead_agegroup_rel.png",
                                                     "data/plots/98_vaccinated_compare_death.png"), in_reply_to_status_id = get_reply_id())


#### run vaccination script

#source("C:\\Rdir\\Rscripts\\43_NICE_vaccine_effect.R")

#### tweet.vaccination.three.tweet ####

tweet.vaccination.three.tweet <- "De Zien-we-al-een-vaccinatie-effect? grafieken
1) Bezetting IC - naar leeftijd
2) Bezetting kliniek - naar leeftijd.
*noot: categorieen verschillen"

tweet.vaccination.three.tweet <- sprintf(tweet.vaccination.three.tweet)
Encoding(tweet.vaccination.three.tweet) <- "UTF-8"
#post_tweet(tweet.vaccination.three.tweet,  media = c("data/03_NICE-IC-leeftijd_relatief.png", 
#                                                   "data/03_NICE-kliniek-leeftijd_relatief.png"), in_reply_to_status_id = get_reply_id())


#### run vaccination carehomes script

#source("C:\\Rdir\\Rscripts\\44_vaccine_effect_care_compare.R")

#### tweet.vaccination.care.tweet ####

tweet.vaccination.care.tweet <- "De Zien-we-al-een-vaccinatie-effect? grafiek
Verschil nieuwe gevallen in verzorgingshuizen vs. de rest.
*noot: de verpleeghuisdata van de laatste dagen is nog niet volledig en daarom niet weergegeven."

tweet.vaccination.care.tweet <- sprintf(tweet.vaccination.care.tweet)
Encoding(tweet.vaccination.care.tweet) <- "UTF-8"
post_tweet(tweet.vaccination.care.tweet,  media = c("data/plots/98_leeftijd_relatief_care.png"), in_reply_to_status_id = get_reply_id())




#### run vaccination age cases script

# source("C:\\Rdir\\Rscripts\\46_vaccine_effect_age_casus_plot.R")

#### tweet.vaccination.age.cases.tweet ####

tweet.vaccination.age.cases.tweet <- "De Zien-we-al-een-vaccinatie-effect? grafiek
Verschil nieuwe gevallen tussen verschillende leeftijdsgroepen."

tweet.vaccination.age.cases.tweet <- sprintf(tweet.vaccination.age.cases.tweet)
Encoding(tweet.vaccination.age.cases.tweet) <- "UTF-8"

post_tweet(tweet.vaccination.age.cases.tweet,  media = c("data/plots/99_leeftijd_relatief_case.png"), in_reply_to_status_id = get_reply_id())


#### tweet.hospital.effect.tweet ####

tweet.hospital.effect.tweet <- "Verschillen opnames tussen verschillende leeftijdsgroepen."

tweet.hospital.effect.tweet <- sprintf(tweet.hospital.effect.tweet)
Encoding(tweet.hospital.effect.tweet) <- "UTF-8"

post_tweet(tweet.hospital.effect.tweet,  media = c("data/plots/70_vaccinated_compare_age_clinic_abs.png",
                                                   "data/plots/70_vaccinated_compare_age_clinic.png",
                                                   "data/plots/71_vaccinated_compare_age_ICU_abs.png",
                                                   "data/plots/71_vaccinated_compare_age_IC.png"
), in_reply_to_status_id = get_reply_id())



#### tweet.vaccination.storage.tweet ####

tweet.vaccination.storage.tweet <- "Hoe lang lagen de geprikte doses in de vriezer?
- aanname: alle types vaccin worden op 1 hoop gegooid."

tweet.vaccination.storage.tweet <- sprintf(tweet.vaccination.storage.tweet)
Encoding(tweet.vaccination.storage.tweet) <- "UTF-8"

post_tweet(tweet.vaccination.storage.tweet,  media = c("data/plots/80_vaccine_on_shelf.png"), in_reply_to_status_id = get_reply_id())




#### data to use



#spillage
#vac.perc.18
#vac.perc.18.second

#days.vaccination.in.nl
vaccins.estimated.total
estimated.new.today
#vaccins.reported.total
people.vaccinated
people.fully.vaccinated
#long.est-long.rep
##


###
ggd.new.today
ha.new.today
care.new.today
#hosp.new.today
##
#est.new.site
#reported.new.today

vac.perc

vac.perc.second

freezer
in.freezer

long.est
long.rep 

