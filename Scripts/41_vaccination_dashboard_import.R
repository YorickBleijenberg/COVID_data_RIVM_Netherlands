

#### import historic vaccination data from GH #####

 second.dose <- 39500

today <- Sys.Date()
yesterday <- today-1

vacc_date_hist.file <- paste0("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/vaccination/daily-dashboard-update/",yesterday,"_vaccine-data.csv")
vacc_date_hist <-read.csv(vacc_date_hist.file,sep=",")
vacc_date_hist$date_of_update <- as.Date(vacc_date_hist$date_of_update)
vacc_date_hist$date <- as.Date(vacc_date_hist$date)
last(vacc_date_hist)


## Locale pulled in from the dashboard repository, with most recent version in branch `master`
locale_json <- "https://raw.githubusercontent.com/minvws/nl-covid19-data-dashboard/master/packages/app/src/locale/nl.json"
locale_dat <- fromJSON(txt = locale_json)
vaccinaties_data_locale <- locale_dat$vaccinaties$data

last(vaccinaties_data_locale$kpi_expected_delivery$value, 1)

## Vaccines delivered

vaccins.geleverd.totaal <- as.integer(vaccinaties_data_locale$kpi_expected_delivery$value)
vaccins.estimated.total <- as.integer(vaccinaties_data_locale$kpi_total$tab_total_estimated$value)
vaccins.reported.total <- as.integer(vaccinaties_data_locale$kpi_total$value)
vaccins.reported <- vaccinaties_data_locale$kpi_total$administered
vaccins.estimated <- vaccinaties_data_locale$kpi_total$tab_total_estimated$administered

vaccins.estimated$value <- as.integer(vaccins.estimated$value)
vaccins.registred.ggd <- vaccins.estimated$value[1]
vaccins.registerd.hops <- vaccins.estimated$value[2]
vaccins.estimated.care <- vaccins.estimated$value[3]

#second.dose <- last(vacc_date_hist$people_fully_vaccinated)

people.vaccinated <- (vaccins.reported.total-second.dose)


estimated.new.today <- (vaccins.estimated.total - last(vacc_date_hist$total_estimated))
reported.new.today <- (vaccins.reported.total - last(vacc_date_hist$total_registerd))
full.new.today <-(second.dose - last(vacc_date_hist$people_fully_vaccinated )) 
ggd.new.today <- (vaccins.registred.ggd - last(vacc_date_hist$ggd_total))
hosp.new.today <- (vaccins.registerd.hops - last(vacc_date_hist$hosp_total ))
care.new.today <- (vaccins.estimated.care - last(vacc_date_hist$care_total))

week <- isoweek(Sys.Date())

new.row.df <- data.frame(today, yesterday,week,vaccins.reported.total,vaccins.estimated.total,people.vaccinated,
                 second.dose,vaccins.registred.ggd,vaccins.registerd.hops, vaccins.estimated.care,
                 reported.new.today,estimated.new.today,full.new.today,ggd.new.today,hosp.new.today,care.new.today)

 colnames(new.row.df) <- c("date_of_update","date","week","total_registerd","total_estimated","people_vaccinated",
                           "people_fully_vaccinated","ggd_total","hosp_total","care_total",
                           "registerd_new","estimated_new","people_fully_vaccinated_new","ggd_new" ,"hosp_new","care_new")

new.vacc.df <- rbind(vacc_date_hist, new.row.df)


daily.vaccination.data.filename <- paste0("data/",format(Sys.time(), "%Y-%m-%d"),"/",format(Sys.time(), "%Y-%m-%d"), "_vaccine-data.csv")
write.csv(new.vacc.df, file = daily.vaccination.data.filename, row.names = F)







####### get data t plot ####

to_play_witch_df <- vacc_date_hist

yesterday_new_ggd <- last(people.vaccinated.ggd.gh$new)
yesterday_new_ggd <- format(yesterday_new_ggd, big.mark="." ,decimal.mark=",")
#subtitle_text_new <- paste("Gisteren gevaccineerd:", yesterday_new_ggd,"\nWe moeten naar 100.000+ per dag (1 miljoen per week)")
#####


key <- "date"
value <- "vacc_total"
#gathercols <- c("Besmettingen","Opnames","Overleden")
gathercols <- c("hosp_new","care_new","ggd_new")
to_play_witch_df.long <- gather(to_play_witch_df, key, value, all_of(gathercols))



to_play_witch_df.long$key <- as.factor(to_play_witch_df.long$key)




ggplot(to_play_witch_df.long)+
  geom_col(aes(x=date, y=value, fill = key ))+
  
  
  
  scale_x_date(date_breaks = "1 weeks", 
               date_labels= format("%d/%m"),
               limits = as.Date(c("2021-01-6", "2021-02-08")))+
  scale_y_continuous(limits = c(0, NA), labels = label_comma(big.mark = ".", decimal.mark = ","))+  #breaks = c(2500, 5000, 7500,10000,12500,15000),
  coord_cartesian(expand = FALSE)+
  theme_classic()+
  xlab("")+
  ylab("")+
  
  labs(title = "Vaccinaties per dag",
      # subtitle = subtitle_text_new,
       caption = paste("Bron: github.com/YorickBleijenberg | Plot: @YorickB  | ",Sys.Date()))+
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    #legend.position = "none",   # no legend
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
  
  ggsave("data/94_vaccinated_new.png",width=16, height = 9)  





key <- "week"
value <- "vacc_total"
#gathercols <- c("Besmettingen","Opnames","Overleden")
gathercols <- c("hosp_new","care_new","ggd_new")
to_play_week_df.long <- gather(to_play_witch_df, key, value, all_of(gathercols))






  ggplot(to_play_week_df.long, aes(x=week, y=value, fill = key ))+
    geom_bar(stat='identity')+
  
  #scale_x_date(date_breaks = "1 weeks", 
  #             date_labels= format("%d/%m"),
  #             limits = as.Date(c("2021-01-6", "2021-02-08")))+
  scale_y_continuous(limits = c(0, NA), labels = label_comma(big.mark = ".", decimal.mark = ","))+  #breaks = c(2500, 5000, 7500,10000,12500,15000),
  coord_cartesian(expand = FALSE)+
  theme_classic()+
  xlab("")+
  ylab("")+
  
  labs(title = "Vaccinaties per week",
       # subtitle = subtitle_text_new,
       caption = paste("Bron: github.com/YorickBleijenberg | Plot: @YorickB  | ",Sys.Date()))+
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    #legend.position = "none",   # no legend
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
  
  ggsave("data/95_vaccinated_week_new.png",width=16, height = 9)  

  
  
  

  
  
  to_play_witch_df$week_day <- weekdays(to_play_witch_df$date)
  
  to_play_witch_df$week_day <- as.factor(to_play_witch_df$week_day)
  
  to_play_witch_df <- to_play_witch_df[to_play_witch_df$date > "2020-06-28",]
  
  to_play_witch_df$weekbegin <- floor_date(to_play_witch_df$date, " week", week_start = 1)
  
  this.week <-floor_date(as.Date(today), " week", week_start = 1) 
  
  to_play_witch_df$new <- to_play_witch_df$ggd_new+to_play_witch_df$hosp_new+ to_play_witch_df$care_new
  
  
  key <- "week"
  value <- "vacc_total"
  #gathercols <- c("Besmettingen","Opnames","Overleden")
  gathercols <- c("hosp_new","care_new","ggd_new")
  to_play_week_df.long <- gather(to_play_witch_df, key, value, all_of(gathercols))
  
  
library(viridis)
  
  
ggplot(to_play_witch_df, aes(x=weekbegin, y=new , fill = factor(week_day, levels=c("Sunday","Saturday","Friday","Thursday", "Wednesday", "Tuesday","Monday"))))+
  
    geom_bar(stat='identity')+
    
    #scale_x_date(date_breaks = "1 weeks", 
    #             date_labels= format("%d/%m"),
    #             limits = as.Date(c("2021-01-6", "2021-02-08")))+
  
    scale_y_continuous(limits = c(0, NA), labels = label_comma(big.mark = ".", decimal.mark = ","))+  #breaks = c(2500, 5000, 7500,10000,12500,15000),
    
  scale_fill_viridis_d() +
  
  coord_cartesian(expand = FALSE)+
    theme_classic()+
    xlab("")+
    ylab("")+
    
    labs(title = "Vaccinaties per week",
         # subtitle = subtitle_text_new,
         caption = paste("Bron: github.com/YorickBleijenberg | Plot: @YorickB  | ",Sys.Date()))+
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
    
ggsave("data/95_vaccinated_week_day.png",width=16, height = 9)  
  
  
  
  
  
  
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

