

library(gganimate)

age.pyramide <-read.csv("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/vaccination/people.vaccinated%20-%20name.known.groups.small.percentage.csv")
age.pyramide <- age.pyramide [,-(5:8)]  


age.pyramide.first <-read.csv("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/vaccination/people.vaccinated%20-%20eerste.prik.perc.csv")
age.pyramide.first <- age.pyramide.first [-(1:31),]
colnames(age.pyramide.first) <- c("week", "12-17",	"18-24",	"25-29",	"30-34",	"35-39",	"40-44",	"45-49",	"50-54",	"55-59",	"60-64",	"65-69",	"70-74",	"75-79",	"80-84",	"85-89", "90+")
age.pyramide.first.long <- gather(age.pyramide.first, key, value, 2:17)
age.pyramide.first.long$week <- as.numeric(age.pyramide.first.long$week)
age.pyramide.first.long$key <- as.factor(age.pyramide.first.long$key)
age.pyramide.first.long$value <- as.numeric(age.pyramide.first.long$value)


age.pyramide.second <-read.csv("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/vaccination/people.vaccinated%20-%20tweede.prik.perc.csv")
age.pyramide.second <- age.pyramide.second [-(1:31),]
colnames(age.pyramide.second) <- c("week", "12-17",	"18-24",	"25-29",	"30-34",	"35-39",	"40-44",	"45-49",	"50-54",	"55-59",	"60-64",	"65-69",	"70-74",	"75-79",	"80-84",	"85-89", "90+")
age.pyramide.second.long <- gather(age.pyramide.second, key, value, 2:17)
age.pyramide.second.long$week <- as.numeric(age.pyramide.second.long$week)
age.pyramide.second.long$key <- as.factor(age.pyramide.second.long$key)
age.pyramide.second.long$value <- as.numeric(age.pyramide.second.long$value)

age.pyramide.number <-read.csv("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/vaccination/people.vaccinated%20-%20prik.perc.number.csv")
age.pyramide.number <- age.pyramide.number [-(1:31),]
colnames(age.pyramide.number) <- c("week", "12-17",	"18-24",	"25-29",	"30-34",	"35-39",	"40-44",	"45-49",	"50-54",	"55-59",	"60-64",	"65-69",	"70-74",	"75-79",	"80-84",	"85-89", "90+")
age.pyramide.number.long <- gather(age.pyramide.number, key, value, 2:17)
age.pyramide.number.long$week <- as.numeric(age.pyramide.number.long$week)
age.pyramide.number.long$key <- as.factor(age.pyramide.number.long$key)
age.pyramide.number.long$value <- as.numeric(age.pyramide.number.long$value)


### animation

animation.vac.perc <- ggplot()+
  geom_col(data = age.pyramide.number.long,  aes( x=value, y=key, fill = "gray"))+
  geom_col(data = age.pyramide.first.long,   aes( x=value, y=key, fill = "#FFD966"))+     #, fill = "#FFD966"))+
  geom_col(data = age.pyramide.second.long,  aes( x=value, y=key, fill = "darkgreen"))+     #, fill = "#FFD966"))+
  
  # geom_col(aes(x=Tweede.prik, y=vac.group, fill = "darkgreen"))+
  
  scale_fill_manual(values=c( "#FFD966","darkgreen", "darkred"), labels=c("Eerste prik", "Volledige vaccinatie", "Niet gevaccineerd" ))+
  scale_x_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  coord_cartesian(expand = FALSE)+
  
  ylab("")+
  xlab("")+
  labs(title="vaccinatie per leeftijdsgroep", 
       # subtitle = "RIVM data.",
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  
  theme(  plot.background = element_rect(fill = "#F5F5F5"),
          plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
          plot.subtitle = element_text(hjust = 0.5,size = 15,face = "italic"),
          panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
          axis.text = element_text(size=14,color = "black",face = "bold"),
          axis.text.y = element_text(face="bold", color="black", size=14),  
          axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
          axis.ticks.length = unit(0.5, "cm"),
          axis.line = element_line(colour = "#F5F5F5"))+ #,
  # panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))+
  
  theme(legend.position =  "top",
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
  
  
transition_time(week)+
labs(subtitle = "Week: {round(frame_time,0)}")

animate(animation.vac.perc, fps=4, end_pause = 25)
anim_save("data/49__vac.age.pyramide.gif",width=16, height = 9)  



#ggsave("data/plots/49_vac.age.pyramyde.png",width=16, height = 9)
