
today <- Sys.Date()
last.year <- today-365

year.compare.cases <- Merged_data_7MA

year.compare.cases <- year.compare.cases [, -c(2:4,6,8:16)]   
year.compare.cases$dateInTable <- as.Date(year.compare.cases$dateInTable)
year.compare.cases$year <- as.Date(year.compare.cases$dateInTable, format= "%Y")
year.compare.cases$year <- format(year.compare.cases$year, format= "%Y")
year.compare.cases$year <- as.factor(year.compare.cases$year)

year.compare.cases$rawDate <- year.compare.cases$dateInTable
year.compare.cases$rawDate <- format(year.compare.cases$rawDate, format = "%m-%d")
year.compare.cases$rawDate <- paste0("2020-", year.compare.cases$rawDate)
year.compare.cases$rawDate <- as.Date(year.compare.cases$rawDate)

year.compare.cases.last.year <- (year.compare.cases %>% filter(dateInTable == last.year ))
year.compare.cases.today <- (year.compare.cases %>% filter(dateInTable == today ))
MAcases.last.year = year.compare.cases.last.year$MACases
MAdead.last.year = year.compare.cases.last.year$MAdead
MAcases.today = year.compare.cases.today$MACases
MAdead.today = year.compare.cases.today$MAdead




MAcases.last.year <-  round(MAcases.last.year, digits =0)
MAcases.today     <-  round(MAcases.today, digits =0)
MAcases.last.year  <- format( MAcases.last.year,  big.mark="." ,decimal.mark=",")
MAcases.today      <- format( MAcases.today,  big.mark="." ,decimal.mark=",")

MAdead.last.year <-  round(MAdead.last.year, digits =1)
MAdead.today     <-  round(MAdead.today, digits =1)
MAdead.last.year  <- format( MAdead.last.year,  big.mark="." ,decimal.mark=",")
MAdead.today      <- format( MAdead.today,  big.mark="." ,decimal.mark=",")




##### plot cases #####



ggplot(year.compare.cases, aes(x=rawDate, y = MACases))+
    geom_line(aes(color = year), lwd=3)+
  scale_color_manual(values = c("#3c81b9", "#e5292b"))+
  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%b"),
               limits = as.Date(c("2020-01-01", "2020-12-31")))+
  scale_y_continuous(limits = c(0, NA), labels = label_comma(big.mark = ".", decimal.mark = ","))+ 
  
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
  labs(title = "Nieuwe gevallen - 2020 vs 2021",
       subtitle = paste("7-daags zwevend gemiddele"),
       caption = paste("bron: RIVM  | Plot: @YorickB | ",Sys.Date()))+

  theme(legend.position ="top",
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.margin = margin(3, 3, 3, 3),
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
  
ggsave("data/plots/11_20.21_compare_cases_all.png",width=16, height = 9)

##### plot deaths #####

ggplot(year.compare.cases, aes(x=rawDate, y = MAdead))+
  geom_line(aes(color = year), lwd=3)+
  scale_color_manual(values = c("#3c81b9", "#e5292b"))+
  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%b"),
               limits = as.Date(c("2020-01-01", "2020-12-31")))+
  scale_y_continuous(limits = c(0, NA))+ 
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
  labs(title = "Nieuwe doden - 2020 vs 2021",
       subtitle = paste("7-daags zwevend gemiddele"),
       caption = paste("bron: RIVM  | Plot: @YorickB | ",Sys.Date()))+

  theme(legend.position = "top",
        legend.background = element_rect(fill="#FDE3E3",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.margin = margin(3, 3, 3, 3),
        legend.text = element_text(colour="black", size=20, face="bold"))+
  
  theme(
    plot.background = element_rect(fill = "#FDE3E3"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#FDE3E3", colour = "#FDE3E3"),
    plot.title = element_text(hjust = 0.5,size = 40,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
    axis.ticks = element_line(colour = "#FDE3E3", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#FDE3E3"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
ggsave("data/plots/11_20.21_compare_dead_all.png",width=16, height = 9)



#### LCPS total occupation ####


LCPS.compare<-read.csv("https://lcps.nu/wp-content/uploads/covid-19-datafeed.csv",sep=",")  
#LCPS_datafeed<-read.csv("C:\\Rdir\\data\\covid-19_lcpsfeed.csv", sep=",")
LCPS.compare$Datum <- as.Date(LCPS.compare$Datum ,format="%d-%m-%Y")

 

LCPS.compare$total.occupation <- LCPS.compare$IC_Bedden_COVID + LCPS.compare$Kliniek_Bedden

LCPS.compare <- LCPS.compare [, -c(2:6)]
LCPS.compare$Datum <-as.Date(LCPS.compare$Datum)
LCPS.compare$year <- format(LCPS.compare$Datum, format= "%Y")
LCPS.compare$year <- as.factor(LCPS.compare$year)

LCPS.compare$rawDate <- LCPS.compare$Datum
LCPS.compare$rawDate <- format(LCPS.compare$rawDate, format = "%m-%d")
LCPS.compare$rawDate <- paste0("2020-", LCPS.compare$rawDate)
LCPS.compare$rawDate <- as.Date(LCPS.compare$rawDate)


year.compare.hosp.occu.last.year <- (LCPS.compare %>% filter(Datum == last.year ))
year.compare.hosp.occu.today <- (LCPS.compare %>% filter(Datum == today ))

MAhospOccu.last.year = year.compare.hosp.occu.last.year$total.occupation
MAhospOccu.today = year.compare.hosp.occu.today$total.occupation


MAhospOccu.last.year <-  round(MAhospOccu.last.year, digits =0)
MAhospOccu.today     <-  round(MAhospOccu.today, digits =0)
MAhospOccu.last.year  <- format( MAhospOccu.last.year,  big.mark="." ,decimal.mark=",")
MAhospOccu.today      <- format( MAhospOccu.today,  big.mark="." ,decimal.mark=",")


ggplot(LCPS.compare, aes(x=rawDate, y = total.occupation))+
  geom_line(aes(color = year), lwd=3)+
  scale_color_manual(values = c("#3c81b9", "#e5292b"))+
  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%b"),
               limits = as.Date(c("2020-01-01", "2020-12-31")))+
  scale_y_continuous(limits = c(0, NA), labels = label_comma(big.mark = ".", decimal.mark = ","))+ 
  
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
  labs(title = "Totale bezetting ziekenhuis - 2020 vs 2021",
       subtitle = paste("Kliniek + IC"),
       caption = paste("bron: LCPS  | Plot: @YorickB | ",Sys.Date()))+
  
  theme(legend.position ="top",
        legend.background = element_rect(fill="#E4ECFC",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.margin = margin(3, 3, 3, 3),
        legend.text = element_text(colour="black", size=20, face="bold"))+
  
  theme(
    plot.background = element_rect(fill = "#E4ECFC"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#E4ECFC", colour = "#E4ECFC"),
    plot.title = element_text(hjust = 0.5,size = 40,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
    axis.ticks = element_line(colour = "#E4ECFC", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#E4ECFC"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
  ggsave("data/plots/11_20.21_compare_hosp.occu_all.png",width=16, height = 9)




#### intake total  ####

NICE.compare <- instroom.combi.year
NICE.compare$total.intake <- NICE.compare$sum_ic + NICE.compare$sum_zkh

NICE.compare <- head(NICE.compare, - 3)   

NICE.compare$total.intake.ma <- rollmeanr(NICE.compare$total.intake, 7, fill = 0)

NICE.compare$date <-as.Date(NICE.compare$date)
NICE.compare$year <- format(NICE.compare$date, format= "%Y")
NICE.compare$year <- as.factor(NICE.compare$year)

NICE.compare$rawDate <- NICE.compare$date
NICE.compare$rawDate <- format(NICE.compare$rawDate, format = "%m-%d")
NICE.compare$rawDate <- paste0("2020-", NICE.compare$rawDate)
NICE.compare$rawDate <- as.Date(NICE.compare$rawDate)



year.compare.hosp.new.last.year <- (NICE.compare %>% filter(date == last.year-3 ))
year.compare.hosp.new.today <- (NICE.compare %>% filter(date == today-3 ))

MAhospNew.last.year = year.compare.hosp.new.last.year$total.intake.ma
MAhospNew.today = year.compare.hosp.new.today$total.intake.ma





MAhospNew.last.year <-  round(MAhospNew.last.year, digits =0)
MAhospNew.today     <-  round(MAhospNew.today, digits =0)
MAhospNew.last.year  <- format( MAhospNew.last.year,  big.mark="." ,decimal.mark=",")
MAhospNew.today      <- format( MAhospNew.today,  big.mark="." ,decimal.mark=",")






ggplot(NICE.compare, aes(x=rawDate, y = total.intake.ma))+
  geom_line(aes(color = year), lwd=3)+
  scale_color_manual(values = c("#3c81b9", "#e5292b"))+
  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%b"),
               limits = as.Date(c("2020-01-01", "2020-12-31")))+
  scale_y_continuous(limits = c(0, NA), labels = label_comma(big.mark = ".", decimal.mark = ","))+ 
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
  labs(title = "Nieuwe opnames (totaal) - 2020 vs 2021",
       subtitle = paste("Kliniek + IC"),
       caption = paste("bron: NICE  | Plot: @YorickB | ",Sys.Date()))+
  
  theme(legend.position ="top",
        legend.background = element_rect(fill="#E4ECFC",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.margin = margin(3, 3, 3, 3),
        legend.text = element_text(colour="black", size=20, face="bold"))+
  
  theme(
    plot.background = element_rect(fill = "#E4ECFC"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#E4ECFC", colour = "#E4ECFC"),
    plot.title = element_text(hjust = 0.5,size = 40,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
    axis.ticks = element_line(colour = "#E4ECFC", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#E4ECFC"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
  ggsave("data/plots/11_20.21_compare_hosp.new_all.png",width=16, height = 9)







tweet.year.compare.all.tweet <- "Vergelijking 2020 - 2021: Hoe staat het er vandaag voor t.o.v. vorig jaar?

Bezetting ziekenhuis: %s --> %s


7-daagse gemiddelden:
- besmettingen: %s --> %s
- doden: %s --> %s
- opnames: %s --> %s
" 


tweet.year.compare.all.tweet <- sprintf(tweet.year.compare.all.tweet,
                                    MAhospOccu.last.year, MAhospOccu.today,
                                    MAcases.last.year,    MAcases.today,
                                    MAdead.last.year,     MAdead.today,
                                    MAhospNew.last.year,  MAhospNew.today
                                    
)
Encoding(tweet.year.compare.all.tweet) <- "UTF-8"


post_tweet(tweet.year.compare.all.tweet,  media = c("data/plots/11_20.21_compare_cases_all.png",
                                           "data/plots/11_20.21_compare_dead_all.png",
                                           "data/plots/11_20.21_compare_hosp.occu_all.png",
                                           "data/plots/11_20.21_compare_hosp.new_all.png"
), in_reply_to_status_id = get_reply_id())






