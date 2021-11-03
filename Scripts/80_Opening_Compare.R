
today <- Sys.Date()
last.year <- today-365

year.compare.cases <- Merged_data_7MA

year.compare.cases <- year.compare.cases [, -c(2:4,6,8:16)]   
year.compare.cases$dateInTable <- as.Date(year.compare.cases$dateInTable)


year.compare.cases$DmJDays <- year.compare.cases$dateInTable -  as.Date("2021-06-26")
year.compare.cases$onefiveDays <- year.compare.cases$dateInTable -  as.Date("2021-09-25")



##### plot cases #####



ggplot(year.compare.cases)+
    geom_line(aes(x=DmJDays, y= MACases, color = "#3c81b9"), lwd=3)+
    geom_line(aes(x=onefiveDays, y= MACases, color = "#e5292b"), lwd=3)+
  
  scale_color_manual(values = c("#e5292b","#3c81b9"),labels=c ("#DansenMetJanssen", "Einde anderhalvemetersamenleving") )+
  
  scale_x_continuous(limits = c(-21, 71), breaks = c(-21,-14,-7,0,7,14,21,28,35,42,49,56,63,70))+
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
  
ggsave("data/plots/10_opening_compare_cases.png",width=16, height = 9)



##### plot deaths #####

ggplot(year.compare.cases)+
  geom_line(aes(x=DmJDays, y= MAdead, color = "#3c81b9"), lwd=3)+
  geom_line(aes(x=onefiveDays, y= MAdead, color = "#e5292b"), lwd=3)+
  
  scale_color_manual(values = c("#e5292b","#3c81b9"),labels=c ("#DansenMetJanssen", "Einde anderhalvemetersamenleving") )+
  
  scale_x_continuous(limits = c(-21, 71), breaks = c(-21,-14,-7,0,7,14,21,28,35,42,49,56,63,70))+
  scale_y_continuous(limits = c(0, 20))+ 
  
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
  
  ggsave("data/plots/10_opening_compare_death.png",width=16, height = 9)


#### LCPS total occupation ####


LCPS.compare<-read.csv("https://lcps.nu/wp-content/uploads/covid-19-datafeed.csv",sep=",")  
#LCPS_datafeed<-read.csv("C:\\Rdir\\data\\covid-19_lcpsfeed.csv", sep=",")
LCPS.compare$Datum <- as.Date(LCPS.compare$Datum ,format="%d-%m-%Y")

 

LCPS.compare$total.occupation <- LCPS.compare$IC_Bedden_COVID + LCPS.compare$Kliniek_Bedden

LCPS.compare <- LCPS.compare [, -c(2:6)]
LCPS.compare$Datum <-as.Date(LCPS.compare$Datum)

LCPS.compare$DmJDays <- LCPS.compare$Datum -  as.Date("2021-06-26")
LCPS.compare$onefiveDays <- LCPS.compare$Datum -  as.Date("2021-09-25")




ggplot(LCPS.compare)+
  geom_line(aes(x=DmJDays, y= total.occupation, color = "#3c81b9"), lwd=3)+
  geom_line(aes(x=onefiveDays, y= total.occupation, color = "#e5292b"), lwd=3)+
  
  scale_color_manual(values = c("#e5292b","#3c81b9"),labels=c ("#DansenMetJanssen", "Einde anderhalvemetersamenleving") )+
  
  scale_x_continuous(limits = c(-21, 71), breaks = c(-21,-14,-7,0,7,14,21,28,35,42,49,56,63,70))+
  scale_y_continuous(limits = c(0, 1000), breaks = c(0,250,500,750,1000))+ 
  
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
  
  ggsave("data/plots/10_opening_compare_total.occu.png",width=16, height = 9)




#### intake total  ####

NICE.compare <- instroom.combi.year
NICE.compare$total.intake <- NICE.compare$sum_ic + NICE.compare$sum_zkh

NICE.compare <- head(NICE.compare, - 3)   

NICE.compare$total.intake.ma <- rollmeanr(NICE.compare$total.intake, 7, fill = 0)

NICE.compare$date <-as.Date(NICE.compare$date)


NICE.compare$DmJDays <- NICE.compare$date -  as.Date("2021-06-26")
NICE.compare$onefiveDays <- NICE.compare$date -  as.Date("2021-09-25")








ggplot(NICE.compare)+
  geom_line(aes(x=DmJDays, y= total.intake.ma, color = "#3c81b9"), lwd=3)+
  geom_line(aes(x=onefiveDays, y= total.intake.ma, color = "#e5292b"), lwd=3)+
  
  scale_color_manual(values = c("#e5292b","#3c81b9"),labels=c ("#DansenMetJanssen", "Einde anderhalvemetersamenleving") )+
  
  scale_x_continuous(limits = c(-21, 71), breaks = c(-21,-14,-7,0,7,14,21,28,35,42,49,56,63,70))+
  scale_y_continuous(limits = c(0, 150), breaks = c(0,25,50,75,100,125,150))+ 
  
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
  
  ggsave("data/plots/10_opening_compare_hosp.new.png",width=16, height = 9)




tweet.opening.compare.tweet <- "Vergelijking
Stap 4+5: #DansenMetJanssen - 26 juni
Stap 6:     Einde anderhalvemetersamenleving, 25 september
" 


tweet.opening.compare.tweet <- sprintf(tweet.opening.compare.tweet)
Encoding(tweet.opening.compare.tweet) <- "UTF-8"

post_tweet(tweet.opening.compare.tweet,  media = c("data/plots/10_opening_compare_cases.png",
                                           "data/plots/10_opening_compare_death.png",
                                           "data/plots/10_opening_compare_total.occu.png",
                                           "data/plots/10_opening_compare_hosp.new.png"
), in_reply_to_status_id = get_reply_id())










