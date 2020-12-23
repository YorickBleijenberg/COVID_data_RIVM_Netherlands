
library(tidyverse)

#### local import ####
##  read.aantal.landelijk.path <- paste("C:\\Rdir\\data\\",Sys.Date(),"\\", Sys.Date(), "_COVID-19_casus_landelijk.csv",sep="")
##  RIVM_casus_landelijk <- read.csv(read.aantal.landelijk.path,sep=";")


weeknumber<-strftime(Sys.Date(),format = "%V")

casus.working <-RIVM_casus_landelijk
casus.working$week<-strftime(casus.working$date,format = "%V")   #adding week_number to the case
casus.working <- casus.working[casus.working$Hospital_admission == 'Yes',]
casus.working = filter(casus.working, Agegroup != "<50" & Agegroup !="Unknown")
casus.working<-count(casus.working,week,Agegroup)
casus.working<- casus.working[casus.working$week>25&casus.working$week<=52,]



#Heatmap
ggplot(casus.working,aes(week,Agegroup,fill=n))+
geom_tile(size=1.5,color="#E4ECFC")+
  geom_text(label=casus.working$n,size=5)+
  scale_fill_gradient2(trans="sqrt",low = "#5B9BD5",mid="#FFEB84",midpoint = 8, 
                       high = "#c00000")+
  theme_minimal()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")+
  labs(title = "Opnames in het ziekenhuis",
       subtitle = "Absolute getallen, binnen de leeftijdsgroep. Week 50, 51 & 52 kunnen nog (sterk) stijgen",fill=NULL,
       caption = paste("Bron data: RIVM | Plot: @YorickB | ",Sys.Date()))+
  theme(plot.background = element_rect(fill = "#E4ECFC"),
        panel.background = element_rect(fill = "#E4ECFC", colour = "#E4ECFC"),
        panel.grid.major = element_line(colour = "#E4ECFC"),
        plot.title = element_text(hjust = 0.5,size = 20,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        axis.text = element_text(size=14,color = "black"),
        axis.ticks = element_line(colour = "#E4ECFC", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.3, "cm"))# +
ggsave("data/02_leeftijd_heatmap-hosp.png",width=16, height = 9)


#Heatmap
ggplot(casus.working,aes(week,Agegroup,fill=n))+
  geom_tile(size=1.5,color="#E4ECFC")+
  geom_text(label=casus.working$n,size=5)+
  scale_fill_gradient2(trans="sqrt",low = "#5B9BD5",mid="#FFEB84",midpoint = 8, 
                       high = "#c00000")+
  theme_minimal()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")+
  labs(title = "Hospitalization",
       subtitle = "Number of cases within each agegroup. Week 51 and 52 will still rise.",fill=NULL,
       caption = paste("Source: RIVM  | Plot: @YorickB | ",Sys.Date()))+
  theme(plot.background = element_rect(fill = "#E4ECFC"),
        panel.background = element_rect(fill = "#E4ECFC", colour = "#E4ECFC"),
        panel.grid.major = element_line(colour = "#E4ECFC"),
        plot.title = element_text(hjust = 0.5,size = 20,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        axis.text = element_text(size=14,color = "black"),
        axis.ticks = element_line(colour = "#E4ECFC", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.3, "cm"))
ggsave("data/02_EN_leeftijd_heatmap-hosp.png",width=16, height = 9)







casus.working <-RIVM_casus_landelijk
casus.working <- casus.working[casus.working$Deceased == 'Yes',]
casus.working$weekOfDead<-substr(casus.working$Week_of_death,5,6)
casus.working<-count(casus.working,weekOfDead,Agegroup)
casus.working <- casus.working[complete.cases(casus.working), ]  #remove N/A
casus.working<- casus.working[casus.working$weekOfDead>25&casus.working$weekOfDead<=51,]



#Heatmap
ggplot(casus.working,aes(weekOfDead,Agegroup,fill=n))+
  geom_tile(size=1.5,color="#FDE3E3")+
  geom_text(label=casus.working$n,size=5)+
  
  scale_fill_gradient2(trans="sqrt",low = "#5B9BD5",mid="#FFEB84",midpoint = 9, 
                       high = "#c00000")+
  ggtitle("Overleden aan COVID-19")+
  theme_minimal()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")+
  labs(title = "Overleden aan COVID-19",
       subtitle = "Absolute getallen, binnen de leeftijdsgroep. Week 50, 51 & 52 kunnen nog sterk stijgen",fill=NULL,
       caption = paste("Bron data: RIVM | Plot: @YorickB | ",Sys.Date()))+
  theme(plot.background = element_rect(fill = "#FDE3E3"),
        panel.background = element_rect(fill = "#FDE3E3", colour = "#FDE3E3"),
        plot.title = element_text(hjust = 0.5,size = 20,face = "bold"),
        panel.grid.major = element_line(colour = "#FDE3E3"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        axis.text = element_text(size=14,color = "black",face = "bold"),
        axis.ticks = element_line(colour = "#FDE3E3", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.3, "cm"))

ggsave("data/02_leeftijd_heatmap-dead.png",width=16, height = 9)



ggplot(casus.working,aes(weekOfDead,Agegroup,fill=n))+
  geom_tile(size=1.5,color="#FDE3E3")+
  geom_text(label=casus.working$n,size=5)+
  scale_fill_gradient2(trans="sqrt",low = "#5B9BD5",mid="#FFEB84",midpoint = 9, 
                       high = "#c00000")+
  ggtitle("Overleden aan COVID-19")+
  theme_minimal()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")+
  labs(title = "Deceased COVID-19",
       subtitle = "Number of deaths, within each agegroup. Week 50, 51 and 52 will still rise.",fill=NULL,
       caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+
  theme(plot.background = element_rect(fill = "#FDE3E3"),
        panel.background = element_rect(fill = "#FDE3E3", colour = "#FDE3E3"),
        panel.grid.major = element_line(colour = "#FDE3E3"),
        plot.title = element_text(hjust = 0.5,size = 20,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        axis.text = element_text(size=14,color = "black",face = "bold"),
        axis.ticks = element_line(colour = "#FDE3E3", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.3, "cm"))
ggsave("data/02_EN_leeftijd_heatmap-dead.png",width=16, height = 9)















