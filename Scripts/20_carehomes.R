

read.verpleeg.path <-paste("C:\\Rdir\\data\\",Sys.Date(),"\\", Sys.Date(), "_COVID-19_verpleeghuizen.csv", sep = "")
copy.verpleeg <- read.csv(read.verpleeg.path,sep=";")

copy.verpleeg$date<-as.Date(copy.verpleeg$Date_of_statistic_reported)   #Adding a date to the case

verpleeg.sm <- copy.verpleeg[ -c(1,2,3,4)]

colnames(verpleeg.sm) <- c("Total_cases","Total_deceased","new_infected_locations","total_infected_locations", "date")

verpleeg.sm <- (verpleeg.sm %>% filter(date > "2020-02-29"))


ggplot(verpleeg.sm)+
  geom_bar(stat = "identity", aes( x=date, y=Total_cases),fill = "#96afde" )+
  
theme_classic()+
  xlab("")+ 
  ylab("")+

  labs(title = "Verpleeghuizen - nieuwe besmettingen",
       #subtitle = paste("7-daags zwevend gemiddele | Actueel tot:", Last_date_in_Google_file, "\n - semi-lockdown op 14 oktober\n - einde herfstvakantie op 25 oktober \n"),
       caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme(legend.position = c(0.7,0.15),
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.margin = margin(3, 3, 3, 3),
        legend.text = element_text(colour="black", size=20, face="bold"))+
  
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         #legend.position = "none",   # no legend
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=12),  #, angle=45),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
         axis.line = element_line(colour = "#F5F5F5"),
         
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
         ### facet label custom
         strip.text.x = element_text(size = 13, color = "black"),
         strip.background = element_rect(color="black", fill="gray", size=1.5, linetype="solid"))+
  ggsave("data/50_Verpleeg_cases.png",width=15, height = 9)





ggplot(verpleeg.sm)+
  geom_bar(stat = "identity", aes( x=date, y=Total_deceased), fill = "#fab0b0")+
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
  labs(title = "Verpleeghuizen - overleden bewoners",
       #subtitle = paste("7-daags zwevend gemiddele | Actueel tot:", Last_date_in_Google_file, "\n - semi-lockdown op 14 oktober\n - einde herfstvakantie op 25 oktober \n"),
       caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme(legend.position = c(0.7,0.15),
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.margin = margin(3, 3, 3, 3),
        legend.text = element_text(colour="black", size=20, face="bold"))+
  
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         #legend.position = "none",   # no legend
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=12),  #, angle=45),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
         axis.line = element_line(colour = "#F5F5F5"),
         
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
         ### facet label custom
         strip.text.x = element_text(size = 13, color = "black"),
         strip.background = element_rect(color="black", fill="gray", size=1.5, linetype="solid"))+
  ggsave("data/51_Verpleeg_dead.png",width=15, height = 9)




ggplot(verpleeg.sm)+
  geom_bar(stat = "identity", aes( x=date, y=total_infected_locations))+
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
  labs(title = "Verpleeghuizen - aantal locaties met een infectie",
       #subtitle = paste("7-daags zwevend gemiddele | Actueel tot:", Last_date_in_Google_file, "\n - semi-lockdown op 14 oktober\n - einde herfstvakantie op 25 oktober \n"),
       caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme(legend.position = c(0.7,0.15),
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.margin = margin(3, 3, 3, 3),
        legend.text = element_text(colour="black", size=20, face="bold"))+
  
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         #legend.position = "none",   # no legend
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=12),  #, angle=45),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
         axis.line = element_line(colour = "#F5F5F5"),
         
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
         ### facet label custom
         strip.text.x = element_text(size = 13, color = "black"),
         strip.background = element_rect(color="black", fill="gray", size=1.5, linetype="solid"))+
  ggsave("data/52_Verpleeg_loc.png",width=14, height = 18)






# verpleeg deaths cumulative

#copy.verpleeg
#copy.verpleeg$Date_of_statistic_reported <-as.Date(copy.verpleeg$Date_of_statistic_reported)
#care.d.cumulative.1 <- copy.verpleeg[copy.verpleeg$Date_of_statistic_reported < "2020-07-01",]
#care.d.cumulative.2 <- copy.verpleeg[copy.verpleeg$Date_of_statistic_reported > "2020-07-01",]

#care.d.cumulative.1a <- sum(care.d.cumulative.1$Total_deceased_reported)
#care.d.cumulative.2b <- sum(care.d.cumulative.2$Total_deceased_reported)

#care.d.perc <- round((care.d.cumulative.2b/care.d.cumulative.1a*100), digits = 1)


