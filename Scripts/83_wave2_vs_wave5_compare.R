
deP <- intToUtf8(0x0025)

today <- Sys.Date()
last.year <- today-365

year.compare.cases <- Merged_data_7MA

year.compare.cases <- year.compare.cases [, -c(2:4,6,8:16)]   
year.compare.cases$dateInTable <- as.Date(year.compare.cases$dateInTable)


year.compare.cases$DmJDays <- year.compare.cases$dateInTable -  as.Date("2020-09-22")
year.compare.cases$onefiveDays <- year.compare.cases$dateInTable -  as.Date("2021-10-06")




rivm_file <- paste0("rivm.file.compare.csv")
write.csv2(year.compare.cases, rivm_file, row.names=FALSE) 


####  percentage calculation cases  ####
then.minus.two.weeks = (today-(365+14))
cases.2020.df <- year.compare.cases[year.compare.cases$date== then.minus.two.weeks,]

cases.2020.date.then <- last(cases.2020.df$dateInTable)
cases.2020.then<- last(cases.2020.df$MACases)

cases.2021.now.date <- last(year.compare.cases$dateInTable)
cases.2021.now <- last(year.compare.cases$MACases)

cases.percentage = round(cases.2021.now/cases.2020.then*100,1)

cases.2020.then <- round(cases.2020.then,0)
cases.2020.then    <- format( cases.2020.then,    big.mark="." ,decimal.mark=",")
cases.2021.now <- round(cases.2021.now,0)
cases.2021.now    <- format( cases.2021.now,    big.mark="." ,decimal.mark=",")


####  percentage calculation deaths  ####

death.2020.then<- last(cases.2020.df$MAdead)
death.2021.now <- last(year.compare.cases$MAdead)

death.percentage = round(death.2021.now/death.2020.then*100,1)

death.2020.then <- round(death.2020.then,1)
death.2020.then    <- format( death.2020.then,    big.mark="," ,decimal.mark=".")
death.2021.now <- round(death.2021.now,1)
death.2021.now    <- format( death.2021.now,    big.mark="," ,decimal.mark=".")





##### plot cases #####



ggplot(year.compare.cases)+
    geom_line(aes(x=DmJDays, y= MACases, color = "#3c81b9"), lwd=3)+
    geom_line(aes(x=onefiveDays, y= MACases, color = "#e5292b"), lwd=3)+
  
  scale_color_manual(values = c("#3c81b9","#e5292b"),labels=c ("herfst 2020", "herfst 2021") )+
  
  scale_x_continuous(limits = c(-21, 180), breaks = c(-21,-14,-7,0,7,14,21,28,35,42,49,56,63,70))+
  scale_y_continuous(limits = c(0, NA))+ 
  
  geom_vline(xintercept=0) +
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
  labs(title = "Besmettingen",
       subtitle = paste("7-daags zwevend gemiddele"),
       caption = paste("bron: RIVM  | Plot: @YorickB | ",Sys.Date()))+
  
  theme(legend.position ="top",
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.margin = margin(3, 3, 3, 3),
        legend.text = element_text(colour="black", size=20, face="bold"),
        legend.direction='vertical')+
  
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title = element_text(hjust = 0.5,size = 35,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
ggsave("data/plots/10_opening_compare_cases_fall.png",width=16, height = 9)



##### plot deaths #####

ggplot(year.compare.cases)+
  geom_line(aes(x=DmJDays, y= MAdead, color = "#3c81b9"), lwd=3)+
  geom_line(aes(x=onefiveDays, y= MAdead, color = "#e5292b"), lwd=3)+
  
  scale_color_manual(values = c("#3c81b9","#e5292b"),labels=c ("herfst 2020", "herfst 2021") )+
  
  scale_x_continuous(limits = c(-21, 71), breaks = c(-21,-14,-7,0,7,14,21,28,35,42,49,56,63,70))+
  scale_y_continuous(limits = c(0, 90))+ 
  
  geom_vline(xintercept=0) +
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
  labs(title = "Overleden",
       subtitle = paste("7-daags zwevend gemiddele"),
       caption = paste("bron: RIVM  | Plot: @YorickB | ",Sys.Date()))+
  
  theme(legend.position ="top",
        legend.background = element_rect(fill="#FDE3E3",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.margin = margin(3, 3, 3, 3),
        legend.text = element_text(colour="black", size=20, face="bold"),
        legend.direction='vertical')+
  
  theme(
    plot.background = element_rect(fill = "#FDE3E3"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#FDE3E3", colour = "#FDE3E3"),
    plot.title = element_text(hjust = 0.5,size = 35,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
    axis.ticks = element_line(colour = "#FDE3E3", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#FDE3E3"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
  ggsave("data/plots/10_opening_compare_death_fall.png",width=16, height = 9)


#### LCPS total occupation ####


LCPS.compare<-read.csv("https://lcps.nu/wp-content/uploads/covid-19-datafeed.csv",sep=",")  
#LCPS_datafeed<-read.csv("C:\\Rdir\\data\\covid-19_lcpsfeed.csv", sep=",")
LCPS.compare$Datum <- as.Date(LCPS.compare$Datum ,format="%d-%m-%Y")

 

LCPS.compare$total.occupation <- LCPS.compare$IC_Bedden_COVID + LCPS.compare$Kliniek_Bedden

LCPS.compare <- LCPS.compare [, -c(2:6)]
LCPS.compare$Datum <-as.Date(LCPS.compare$Datum)

LCPS.compare$DmJDays <- LCPS.compare$Datum -  as.Date("2020-09-22")
LCPS.compare$onefiveDays <- LCPS.compare$Datum -  as.Date("2021-10-06")



rivm_file <- paste0("LCPS.file.compare.csv")
write.csv2(LCPS.compare, rivm_file, row.names=FALSE) 



####  percentage calculation occu  ####
occu.2020.df <- LCPS.compare[LCPS.compare$Datum== then.minus.two.weeks,]

occu.2020.then<- last(occu.2020.df$total.occupation)
occu.2021.now <- first(LCPS.compare$total.occupation)
occu.percentage = round(occu.2021.now/occu.2020.then*100,1)

occu.2020.then    <- format( occu.2020.then,    big.mark="." ,decimal.mark=",")
occu.2021.now    <- format( occu.2021.now,    big.mark="." ,decimal.mark=",")




ggplot(LCPS.compare)+
  geom_line(aes(x=DmJDays, y= total.occupation, color = "#3c81b9"), lwd=3)+
  geom_line(aes(x=onefiveDays, y= total.occupation, color = "#e5292b"), lwd=3)+
  
  scale_color_manual(values = c("#3c81b9","#e5292b"),labels=c ("herfst 2020", "herfst 2021") )+
  
  scale_x_continuous(limits = c(-21, 71), breaks = c(-21,-14,-7,0,7,14,21,28,35,42,49,56,63,70))+
  scale_y_continuous(limits = c(0, 3000), breaks = c(0,250,500,750,1000,1500,2000,2500,3000))+ 
  
  geom_vline(xintercept=0) +
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
  labs(title = "Bezetting Ziekenhuis, IC + kliniek",
       subtitle = paste("7-daags zwevend gemiddele"),
       caption = paste("bron: LCPS  | Plot: @YorickB | ",Sys.Date()))+
  
  theme(legend.position ="top",
        legend.background = element_rect(fill="#E4ECFC",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.margin = margin(3, 3, 3, 3),
        legend.text = element_text(colour="black", size=20, face="bold"),
        legend.direction='vertical')+
  
  theme(
    plot.background = element_rect(fill = "#E4ECFC"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#E4ECFC", colour = "#E4ECFC"),
    plot.title = element_text(hjust = 0.5,size = 35,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
    axis.ticks = element_line(colour = "#E4ECFC", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#E4ECFC"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
  ggsave("data/plots/10_opening_compare_total.occu_fall.png",width=16, height = 9)




#### intake total  ####

NICE.compare <- instroom.combi.year
NICE.compare$total.intake <- NICE.compare$sum_ic + NICE.compare$sum_zkh

NICE.compare <- head(NICE.compare, - 3)   

NICE.compare$total.intake.ma <- rollmeanr(NICE.compare$total.intake, 7, fill = 0)

NICE.compare$date <-as.Date(NICE.compare$date)


NICE.compare$DmJDays <- NICE.compare$date -  as.Date("2020-09-22")
NICE.compare$onefiveDays <- NICE.compare$date -  as.Date("2021-10-06")




rivm_file <- paste0("NICE.file.compare.csv")
write.csv2(NICE.compare, rivm_file, row.names=FALSE) 


####  percentage calculation new  ####
nice.new.2020.df <- NICE.compare[NICE.compare$date== then.minus.two.weeks-3,]

nice.new.2020.then <- last(nice.new.2020.df$total.intake.ma)
nice.new.2021.now <- last(NICE.compare$total.intake.ma)
nice.new.percentage = round(nice.new.2021.now/nice.new.2020.then*100,1)

nice.new.2020.then <- round(nice.new.2020.then,0)
nice.new.2021.now  <- round(nice.new.2021.now,0)

nice.new.2020.then    <- format( nice.new.2020.then,    big.mark="." ,decimal.mark=",")
nice.new.2021.now    <- format( nice.new.2021.now,    big.mark="." ,decimal.mark=",")



ggplot(NICE.compare)+
  geom_line(aes(x=DmJDays, y= total.intake.ma, color = "#3c81b9"), lwd=3)+
  geom_line(aes(x=onefiveDays, y= total.intake.ma, color = "#e5292b"), lwd=3)+
  
  scale_color_manual(values = c("#3c81b9","#e5292b"),labels=c ("herfst 2020", "herfst 2021") )+
  
  scale_x_continuous(limits = c(-21, 71), breaks = c(-21,-14,-7,0,7,14,21,28,35,42,49,56,63,70))+
  scale_y_continuous(limits = c(0, 350))+ 
  
  geom_vline(xintercept=0) +
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
  labs(title = "Nieuwe opnames, totaal",
       subtitle = paste("7-daags zwevend gemiddele"),
       caption = paste("bron: NICE  | Plot: @YorickB | ",Sys.Date()))+
  
  theme(legend.position ="top",
        legend.background = element_rect(fill="#E4ECFC",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.margin = margin(3, 3, 3, 3),
        legend.text = element_text(colour="black", size=20, face="bold"),
        legend.direction='vertical')+
  
  theme(
    plot.background = element_rect(fill = "#E4ECFC"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#E4ECFC", colour = "#E4ECFC"),
    plot.title = element_text(hjust = 0.5,size = 35,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
    axis.ticks = element_line(colour = "#E4ECFC", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#E4ECFC"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
  ggsave("data/plots/10_opening_compare_hosp.new_fall.png",width=16, height = 9)




tweet.fall.compare.tweet <- "%s vs. %s
(14 dagen verschuiving voor beste vergelijking)

Cases: %s - %s
(%s%s)
Doden: %s - %s
(%s%s)
Bezetting: %s - %s
(%s%s)
Opnames: %s - %s
(%s%s)

Verschil:
- Andere variant
- Vaccinaties
- Ander maatregelenpakket."

tweet.fall.compare.tweet <- sprintf(tweet.fall.compare.tweet,
                                    
                                    cases.2020.date.then, cases.2021.now.date,
                                    
                                    cases.2020.then, cases.2021.now,cases.percentage, deP,
                                    death.2020.then,  death.2021.now, death.percentage, deP,
                                    occu.2020.then,occu.2021.now, occu.percentage, deP,
                                    nice.new.2020.then, nice.new.2021.now, nice.new.percentage, deP
                                    
                                    
                                    )
Encoding(tweet.fall.compare.tweet) <- "UTF-8"

post_tweet(tweet.fall.compare.tweet,  media = c("data/plots/10_opening_compare_cases_fall.png",
                                           "data/plots/10_opening_compare_death_fall.png",
                                           "data/plots/10_opening_compare_total.occu_fall.png",
                                           "data/plots/10_opening_compare_hosp.new_fall.png"
) , in_reply_to_status_id = get_reply_id())










