
### kerstdoden ###

read.aantal.landelijk.path <- paste("C:\\Rdir\\data\\",Sys.Date(),"\\", Sys.Date(), "_COVID-19_casus_landelijk.csv",sep="")

copy.aantal.landelijk.gem.dag <- read.csv(read.aantal.landelijk.path,sep=";")

christ.death <- copy.aantal.landelijk.gem.dag[,-c(1,4:6,10,11)]
christ.death$date <- as.Date(christ.death$date)

christ.death <- christ.death[christ.death$Date_statistics>"2020-12-30"&christ.death$Date_statistics<="2021-01-02",]  #date of even plus 5-7 days
christ.death.hosp <- christ.death[christ.death$Hospital_admission == 'Yes',]
christ.death.dead <- christ.death[christ.death$Deceased == 'Yes',]

total.infected = nrow(christ.death)
total.hopitalized = nrow(christ.death.hosp)
total.dead = nrow(christ.death.dead)

total.infected  <- format(total.infected, big.mark="." ,decimal.mark=",")
total.hopitalized  <- format( total.hopitalized, big.mark="." ,decimal.mark=",")
total.dead  <- format( total.dead, big.mark="." ,decimal.mark=",")

total.infected <- paste(total.infected, "mensen besmet")
total.hopitalized <- paste(total.hopitalized, "opgenomen")
total.dead <- paste("Mensen overleden:", total.dead)


placeholder <- data.frame(x = 0:16, y = 0:16)


ggplot(placeholder)+
  geom_area(aes(x=x,y=y), fill = "#F5F5F5")+
  geom_text(mapping=aes(x=1, y=14, label=total.infected), size=15, angle=0, vjust=0, hjust=0, color= "darkblue")+
  geom_text(mapping=aes(x=4, y=8, label=total.hopitalized), size=20, angle=0, vjust=0, hjust=0, color= "black")+
  geom_text(mapping=aes(x=4, y=1, label=total.dead), size=25, angle=0, vjust=0, hjust=0, color= "darkred")+
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  labs(title = "kerstdoden",
       subtitle = "Datum eerste klachten tussen 30 december en 2 januari",
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"),
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    legend.position = "none",   
    plot.title = element_text(hjust = 0.5,size = 40,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text=element_blank(),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.line = element_line(colour = "#F5F5F5")
  
   )

ggsave("data/88_christ_death.png",width=16, height = 9)  

