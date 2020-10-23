#library(cbsodataR)
#library(sf)
#library(dplyr)
#library(extrafont)
#library(ggplot2)
library(tidyverse)
library(RcppRoll)
#require(data.table)


#bla <-cases_per_day
bla <-RIVM_casus_landelijk
#copy.aantal.landelijk.gem.dag$week<-strftime(copy.aantal.landelijk.gem.dag$date,format = "%V")


bla2 <- bla
bla2 <- bla2[bla2$Hospital_admission == 'Yes',]

bla3 <- bla
bla3 <- bla3[bla3$Deceased == 'Yes',]


bla3$weekOfDead<-substr(bla3$Week_of_death,5,6)


bla2_2<-count(bla2,week,Agegroup)
bla3_3<-count(bla3,weekOfDead,Agegroup)


bla3_3 <- bla3_3[complete.cases(bla3_3), ]  #remove N/A

#Aantal per week per groep tellen + leeftijdverdeling landelijk pakken
#mergen + per honderduizen berekenen
#bla   <- merge(bla,CBS_age_10yrs_GH)
#bla$phd <- round(bla$n*100000/bla$population,0)

#weeknumber <- isoweek(Sys.Date())
weeknumber<-strftime(Sys.Date(),format = "%V")


bla2_2<- bla2_2[bla2_2$week>25&bla2_2$week<=43,]
bla3_3<- bla3_3[bla3_3$weekOfDead>25&bla3_3$weekOfDead<=43,]




#Heatmap
ggplot(bla2_2,aes(week,Agegroup,fill=n))+
geom_tile(size=1.5,color="#E4ECFC")+
  geom_text(label=bla2_2$n,size=5)+
  scale_fill_gradient2(trans="sqrt",low = "#5B9BD5",mid="#FFEB84",midpoint = 5, 
                       high = "#c00000")+
  theme_minimal()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")+
  labs(title = "Opnames in het ziekenhuis",
       subtitle = "Absolute getallen, binnen de leeftijdsgroep. Week 41, 42 & 43 kunnen nog sterk stijgen",fill=NULL,
       caption = paste("Bron data: RIVM, ",Sys.Date()))+
  theme(plot.background = element_rect(fill = "#E4ECFC"),
        panel.background = element_rect(fill = "#E4ECFC", colour = "#E4ECFC"),
        plot.title = element_text(hjust = 0.5,size = 20,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        axis.text = element_text(size=14,color = "black"),
        axis.ticks = element_line(colour = "#E4ECFC", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.3, "cm"))# +
ggsave("data/02_leeftijd_heatmap-hosp.png",width=16, height = 9)




#Heatmap
ggplot(bla2_2,aes(week,Agegroup,fill=n))+
  geom_tile(size=1.5,color="#E4ECFC")+
  geom_text(label=bla2_2$n,size=5)+
  scale_fill_gradient2(trans="sqrt",low = "#5B9BD5",mid="#FFEB84",midpoint = 5, 
                       high = "#c00000")+
  theme_minimal()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")+
  labs(title = "Hospitalization",
       subtitle = "Number of cases within each agegroup. Week 42 and 43 will still rise.",fill=NULL,
       caption = paste("Source: RIVM  | Plot: @YorickB | ",Sys.Date()))+
  theme(plot.background = element_rect(fill = "#E4ECFC"),
        panel.background = element_rect(fill = "#E4ECFC", colour = "#E4ECFC"),
        plot.title = element_text(hjust = 0.5,size = 20,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        axis.text = element_text(size=14,color = "black"),
        axis.ticks = element_line(colour = "#E4ECFC", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.3, "cm"))# +
ggsave("data/02_EN_leeftijd_heatmap-hosp.png",width=16, height = 9)











#Heatmap
ggplot(bla3_3,aes(weekOfDead,Agegroup,fill=n))+
  geom_tile(size=1.5,color="#FDE3E3")+
  geom_text(label=bla3_3$n,size=5)+
  scale_fill_gradient2(trans="sqrt",low = "#5B9BD5",mid="#FFEB84",midpoint = 5, 
                       high = "#c00000")+
  ggtitle("Overleden aan COVID-19")+
  theme_minimal()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")+
  labs(title = "Overleden aan COVID-19",
       subtitle = "Absolute getallen, binnen de leeftijdsgroep. Week 41, 42 & 43 kunnen nog sterk stijgen",fill=NULL,
       caption = paste("Bron data: RIVM, ",Sys.Date()))+
  theme(plot.background = element_rect(fill = "#FDE3E3"),
        panel.background = element_rect(fill = "#FDE3E3", colour = "#FDE3E3"),
        plot.title = element_text(hjust = 0.5,size = 20,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        axis.text = element_text(size=14,color = "black",face = "bold"),
        axis.ticks = element_line(colour = "#FDE3E3", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.3, "cm"))# +
ggsave("data/02_leeftijd_heatmap-dead.png",width=16, height = 9)



ggplot(bla3_3,aes(weekOfDead,Agegroup,fill=n))+
  geom_tile(size=1.5,color="#FDE3E3")+
  geom_text(label=bla3_3$n,size=5)+
  scale_fill_gradient2(trans="sqrt",low = "#5B9BD5",mid="#FFEB84",midpoint = 5, 
                       high = "#c00000")+
  ggtitle("Overleden aan COVID-19")+
  theme_minimal()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")+
  labs(title = "Deceased COVID-19",
       subtitle = "Number of deaths, within each agegroup. Week 42 and 43 will still rise.",fill=NULL,
       caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+
  theme(plot.background = element_rect(fill = "#FDE3E3"),
        panel.background = element_rect(fill = "#FDE3E3", colour = "#FDE3E3"),
        plot.title = element_text(hjust = 0.5,size = 20,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        axis.text = element_text(size=14,color = "black",face = "bold"),
        axis.ticks = element_line(colour = "#FDE3E3", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.3, "cm"))# +
ggsave("data/02_EN_leeftijd_heatmap-dead.png",width=16, height = 9)















