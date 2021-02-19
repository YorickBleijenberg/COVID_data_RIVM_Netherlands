



vaccine.edition.name <- "kan-iedereen-stoppen-met-zeuren-over-de-sneheid-in-de-mentions-aub?-alvast-bedankt!"
second.dose <- 40000+30000+152253
vaccinated.second <- second.dose
freezer = 1378700


#### import historic vaccination data from GH #####

today <- Sys.Date()
yesterday <- today-1

vacc_date_hist.file <- paste0("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/vaccination/daily-dashboard-update/",yesterday,"_vaccine-data.csv")
vacc_date_hist <-read.csv(vacc_date_hist.file,sep=",")
vacc_date_hist$date_of_update <- as.Date(vacc_date_hist$date_of_update)
vacc_date_hist$date <- as.Date(vacc_date_hist$date)
last(vacc_date_hist$total_estimated)



## Locale pulled in from the dashboard repository, with most recent version in branch `master`
locale_json <- "https://coronadashboard.rijksoverheid.nl/json/NL.json"
locale_dat <- fromJSON(txt = locale_json)

as.Date(as.POSIXct(locale_dat$vaccine_administered_total$last_value$date_unix , origin="1970-01-01"))
locale_dat$vaccine_administered_total$last_value$estimated
locale_dat$vaccine_administered_total$last_value$reported


#test.df <- locale_dat$vaccine_administered_ggd$values
#test.df$date_unix <- as.Date(as.POSIXct(test.df$date_unix , origin="1970-01-01"))
#daily.vaccination.data.filename <- paste0("data/",format(Sys.time(), "%Y-%m-%d"),"/",format(Sys.time(), "%Y-%m-%d"), "_vaccine-data-lnaz.csv")
#write.csv(test.df, file = daily.vaccination.data.filename, row.names = F)


## Vaccines given

vaccins.estimated.total <- as.integer(locale_dat$vaccine_administered_total$last_value$estimated)
vaccins.reported.total  <- as.integer(locale_dat$vaccine_administered_total$last_value$reported)
vaccins.registred.ggd   <- as.integer(locale_dat$vaccine_administered_ggd_ghor$last_value$reported)
vaccins.registerd.hops  <- as.integer(locale_dat$vaccine_administered_lnaz$last_value$reported)
vaccins.estimated.care  <- as.integer(locale_dat$vaccine_administered_care_institutions$last_value$estimated)
vaccins.estimated.ha    <- as.integer(locale_dat$vaccine_administered_doctors$last_value$estimated)

#second.dose <- last(vacc_date_hist$people_fully_vaccinated)

people.vaccinated <- (vaccins.estimated.total-second.dose)


estimated.new.today <- (vaccins.estimated.total - last(vacc_date_hist$total_estimated))
reported.new.today <- (vaccins.reported.total - last(vacc_date_hist$total_registerd))
full.new.today <-(second.dose - last(vacc_date_hist$people_fully_vaccinated )) ### hier gaat iets mis
ggd.new.today <- (vaccins.registred.ggd - last(vacc_date_hist$ggd_total))
hosp.new.today <- (vaccins.registerd.hops - last(vacc_date_hist$hosp_total ))
care.new.today <- (vaccins.estimated.care - last(vacc_date_hist$care_total))
ha.new.today <- (vaccins.estimated.ha - last(vacc_date_hist$ha_total))

week <- isoweek(Sys.Date()-1)

new.row.df <- data.frame(today, yesterday,week,vaccins.reported.total,vaccins.estimated.total,people.vaccinated,
                 second.dose,vaccins.registred.ggd,vaccins.registerd.hops, vaccins.estimated.care,vaccins.estimated.ha,
                 reported.new.today,estimated.new.today,full.new.today,ggd.new.today,hosp.new.today,care.new.today,ha.new.today)

 colnames(new.row.df) <- c("date_of_update","date","week","total_registerd","total_estimated","people_vaccinated",
                           "people_fully_vaccinated","ggd_total","hosp_total","care_total","ha_total",
                           "registerd_new","estimated_new","people_fully_vaccinated_new","ggd_new" ,"hosp_new","care_new","ha_new")

new.vacc.df <- rbind(vacc_date_hist, new.row.df)


daily.vaccination.data.filename <- paste0("data/",format(Sys.Date(), "%Y-%m-%d"),"/",format(Sys.Date(), "%Y-%m-%d"), "_vaccine-data.csv")
write.csv(new.vacc.df, file = daily.vaccination.data.filename, row.names = F)





####### get data t plot ####

to_play_witch_df <- new.vacc.df

to_play_witch_df$for_MA <- to_play_witch_df$care_new+to_play_witch_df$hosp_new+to_play_witch_df$ggd_new

to_play_witch_df$MAnew  <- rollmeanr(to_play_witch_df$for_MA, 7, fill = 0)
to_play_witch_df$MAnew_lead  <- lead(  to_play_witch_df$MAnew,3)

s_dayMA_tot <- last(to_play_witch_df$MAnew)
s_dayMA_tot <- format(as.integer(s_dayMA_tot) ,big.mark = ".", decimal.mark = ",")


#yesterday_new_ggd <- last(people.vaccinated.ggd.gh$new)
#yesterday_new_ggd <- format(yesterday_new_ggd, big.mark="." ,decimal.mark=",")
#subtitle_text_new <- paste("Gisteren gevaccineerd:", yesterday_new_ggd,"\nWe moeten naar 100.000+ per dag (1 miljoen per week)")
#####


key <- "date"
value <- "vacc_total"
#gathercols <- c("Besmettingen","Opnames","Overleden")
gathercols <- c("hosp_new","care_new","ggd_new", "ha_new")
to_play_witch_df.long <- gather(to_play_witch_df, key, value, all_of(gathercols))



to_play_witch_df.long$key <- as.factor(to_play_witch_df.long$key)




ggplot(to_play_witch_df.long)+
  geom_col(aes(x=date, y=value, fill = key ))+
  geom_line( aes(x=date, y=MAnew_lead), color = "#f5f5f5", size = 4)+
  geom_line( aes(x=date, y=MAnew_lead), color = "black", size = 2)+  
  scale_x_date(date_breaks = "1 weeks", 
               date_labels= format("%d/%m"),
               limits = as.Date(c("2021-01-6", NA)))+
  scale_y_continuous(limits = c(0, NA), labels = label_comma(big.mark = ".", decimal.mark = ","))+ 
  coord_cartesian(expand = FALSE)+
  theme_classic()+
  xlab("")+
  ylab("")+
  
  scale_fill_manual( values=c("#5c146e", "#fca50a", "green", "#dd513a"), labels=c("zorginstellingen", "GGD'en","Huisartsen", "ziekenhuizen" ))+
  
  labs(title = "Vaccinaties per dag",
      # subtitle = subtitle_text_new,
       caption = paste("Bron: github.com/YorickBleijenberg | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(#legend.position = "none",   # no legend
    legend.title = element_blank(),  ## legend title
    legend.position="top",
    legend.background = element_rect(fill="#f5f5f5", size=0.5, linetype="solid")
    )+
  
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





key <- "week"
value <- "vacc_total"
#gathercols <- c("Besmettingen","Opnames","Overleden")
gathercols <- c("hosp_new","care_new","ggd_new", "ha_new")
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
  
    scale_fill_manual( values=c("#5c146e", "#fca50a", "green", "#dd513a"), labels=c("zorginstellingen", "GGD'en","Huisartsen", "ziekenhuizen" ))+
    
  labs(title = "Vaccinaties per week",
       # subtitle = subtitle_text_new,
       caption = paste("Bron: github.com/YorickBleijenberg | Plot: @YorickB  | ",Sys.Date()))+
    
    theme(#legend.position = "none",   # no legend
      legend.title = element_blank(),  ## legend title
      legend.position="top",
      legend.background = element_rect(fill="#f5f5f5", size=0.5, linetype="solid")
    )+
    
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
  
  ggsave("data/plots/95_vaccinated_week_new.png",width=16, height = 9)  

  
  
  
  
  to_play_witch_df$week_day <- weekdays(to_play_witch_df$date)
  
  to_play_witch_df$week_day <- as.factor(to_play_witch_df$week_day)
  
  to_play_witch_df <- to_play_witch_df[to_play_witch_df$date > "2020-06-28",]
  
  to_play_witch_df$weekbegin <- floor_date(to_play_witch_df$date, " week", week_start = 1)
  
  this.week <-floor_date(as.Date(today), " week", week_start = 1) 
  
  to_play_witch_df$new <- to_play_witch_df$ggd_new+to_play_witch_df$hosp_new+ to_play_witch_df$care_new
  
  
  key <- "week"
  value <- "vacc_total"
  #gathercols <- c("Besmettingen","Opnames","Overleden")
  gathercols <- c("hosp_new","care_new","ggd_new", "ha_new")
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
    
ggsave("data/plots/95_vaccinated_week_day.png",width=16, height = 9)  
  
  
  


agecbs.df <-read.csv("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/data-cbs/people.vaccinated%20-%20age-reverse.csv",sep=",")




ggplot(new.vacc.df)+
  geom_col(aes(x= date, y=total_registerd))+
  
  geom_hline(data=agecbs.df, mapping=aes(yintercept=aantal), color="black")+
  geom_text(data=agecbs.df, mapping=aes(x=as.Date("2021-01-01"), y=aantal, label=Leeftijd), size=4, vjust=-0.4, hjust=0)+
  
  scale_y_continuous(limits = c(0, 18000000),breaks = c(5000000,10000000,15000000), labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  ggsave("data/92_vaccine_age_full.png",width=5, height = 25)


ggplot(new.vacc.df)+
  geom_col(aes(x= date, y=total_registerd))+
  geom_col(aes(x= date, y=total_estimated))+
  
  geom_hline(data=agecbs.df, mapping=aes(yintercept=aantal), color="black")+
  geom_text(data=agecbs.df, mapping=aes(x=as.Date("2021-01-01"), y=aantal, label=Leeftijd), size=4, vjust=-0.4, hjust=0)+
  
  scale_y_continuous(limits = c(0, 1000000),breaks = c(5000000,10000000,15000000), labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  ggsave("data/92_vaccine_age_small.png",width=16, height = 9)
  
  



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

spillage <- as.integer(vaccinated.people.total/95)*5
in.freezer <- freezer-vaccinated.people.total-spillage


days.vaccination.in.nl
vaccins.estimated.total
vaccins.reported.total
in.freezer
spillage
vaccinated.people.total
#vac.perc.18
vac.perc
#vac.perc.18.second
vac.perc.second

people.vaccinated
second.dose


vaccins.estimated.total <- format(vaccins.estimated.total,big.mark = ".", decimal.mark = ",")
vaccins.reported.total <- format(vaccins.reported.total ,big.mark = ".", decimal.mark = ",")

estimated.new.today <- format(estimated.new.today,big.mark = ".", decimal.mark = ",")
reported.new.today <- format(reported.new.today ,big.mark = ".", decimal.mark = ",")

vaccins.estimated.total
vaccins.reported.total
estimated.new.today

require(magick);
require(stringr);

banner.text <- paste(" ",vaccins.estimated.total,"\n\n", vaccins.reported.total)

banner <- image_read('https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/background/background.png') %>%
  
image_annotate(banner.text , font = 'Cabin', weight = '600', size = 60,  location = "+0+25", gravity = "center", color = "white")#%>%
#image_annotate( vaccins.reported.total, font = 'Cabin', weight = '600', size = 40,  location = "+0+80", gravity = "center", color = "white") %>%
image_write(banner, str_c("data/",Sys.Date(),"-banner.png") )




##### set edition name  ###



emoji_syringe <- intToUtf8(0x1F489)


#### tweet.vaccination.start.tweet ####

tweet.vaccination.start.tweet <- "%sVaccinatiedag %s, De %s editie.%s

Totaal geschat:           %s (+%s)  
Totaal geregistreerd:  %s (+%s)

7-daags gemiddelde: %s per dag.



coronabeeld.nl
"


tweet.vaccination.start.tweet <- sprintf(tweet.vaccination.start.tweet,
                                         emoji_syringe, days.vaccination.in.nl, vaccine.edition.name,emoji_syringe,
                                         vaccins.estimated.total,estimated.new.today,
                                         vaccins.reported.total,reported.new.today,
                                         s_dayMA_tot
                                   )
Encoding(tweet.vaccination.start.tweet) <- "UTF-8"

baner.path<- paste0("data/",Sys.Date(),"-banner.png")

post_tweet(tweet.vaccination.start.tweet,  media = c(baner.path))



#### tweet.vaccination.two.tweet ####

tweet.vaccination.two.tweet <- "vaccinaties per:
- Dag van de week
- Organistatie, per week
- Organistatie, per dag"
tweet.vaccination.two.tweet <- sprintf(tweet.vaccination.two.tweet)
Encoding(tweet.vaccination.two.tweet) <- "UTF-8"
post_tweet(tweet.vaccination.two.tweet,  media = c("data/plots/95_vaccinated_week_new.png", 
                                                     "data/plots/95_vaccinated_week_day.png", 
                                                     "data/plots/94_vaccinated_new.png"), in_reply_to_status_id = get_reply_id())


#### run vaccintation script

source("C:\\Rdir\\Rscripts\\43_NICE_vaccine_effect.R")

#### tweet.vaccination.three.tweet ####

tweet.vaccination.three.tweet <- "De Zien-we-al-een-vaccinatie-effect? grafieken
1) Bezetting IC - naar leeftijd
2) Bezetting kliniek - naar leeftijd.

*noot: categorieen verschillen"

tweet.vaccination.three.tweet <- sprintf(tweet.vaccination.three.tweet)
Encoding(tweet.vaccination.three.tweet) <- "UTF-8"
post_tweet(tweet.vaccination.three.tweet,  media = c("data/03_NICE-IC-leeftijd_relatief.png", 
                                                   "data/03_NICE-kliniek-leeftijd_relatief.png"), in_reply_to_status_id = get_reply_id())






