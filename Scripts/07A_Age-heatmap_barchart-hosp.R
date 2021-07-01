
library(tidyverse)

#### local import ####
##  read.aantal.landelijk.path <- paste("C:\\Rdir\\data\\",Sys.Date(),"\\", Sys.Date(), "_COVID-19_casus_landelijk.csv",sep="")
##  RIVM_casus_landelijk <- read.csv(read.aantal.landelijk.path,sep=";")


weeknumber<-strftime(Sys.Date(),format = "%V")

casus.working <-RIVM_casus_landelijk
casus.working$week<-strftime(casus.working$date,format = "%V")   #adding week_number to the case

casus.working$weekbegin <- floor_date(casus.working$date, " week", week_start = 1)

casus.working <- casus.working[casus.working$Hospital_admission == 'Yes',]
casus.working = filter(casus.working, Agegroup != "<50" & Agegroup !="Unknown")
casus.working<-count(casus.working,weekbegin,Agegroup)
casus.working<- casus.working[casus.working$weekbegin>"2020-07-01"&casus.working$weekbegin<=today,]



#Heatmap
ggplot(casus.working,aes(weekbegin,Agegroup,fill=n))+
geom_tile(size=1.5,color="#E4ECFC")+
  geom_text(label=casus.working$n,size=5, angle=90)+
  scale_fill_gradient2(trans="sqrt",low = "#5B9BD5",mid="#FFEB84",midpoint = 8, 
                       high = "#c00000")+
  scale_x_date(as.Date("2020-07-06"),breaks = "1 week",  labels = date_format("%V"))+
  coord_cartesian(expand = FALSE)+
  theme_minimal()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")+
  labs(title = "Opnames in het ziekenhuis",
       subtitle = "Absolute getallen, binnen de leeftijdsgroep. Week 25 & 26 kunnen nog (sterk) stijgen",fill=NULL,
       caption = paste("Bron data: RIVM | Plot: @YorickB | ",Sys.Date()))+
  theme(plot.background = element_rect(fill = "#E4ECFC"),
        panel.background = element_rect(fill = "#E4ECFC", colour = "#E4ECFC"),
        panel.grid.major = element_line(colour = "#E4ECFC"),
        plot.title = element_text(hjust = 0.5,size = 20,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        axis.text = element_text(size=14,color = "black", face = "bold"),
        axis.ticks = element_line(colour = "#E4ECFC", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.3, "cm"),
        axis.title.x=element_blank())# +
ggsave("data/02_leeftijd_heatmap-hosp.png",width=16, height = 9)


#Heatmap
ggplot(casus.working,aes(weekbegin,Agegroup,fill=n))+
  geom_tile(size=1.5,color="#E4ECFC")+
  geom_text(label=casus.working$n,size=5, angle=90)+
  scale_fill_gradient2(trans="sqrt",low = "#5B9BD5",mid="#FFEB84",midpoint = 8, 
                       high = "#c00000")+
  scale_x_date(as.Date("2020-07-06"),breaks = "1 week",  labels = date_format("%V"))+
  coord_cartesian(expand = FALSE)+
  theme_minimal()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")+
  labs(title = "Hospitalization",
       subtitle = "Number of cases within each agegroup. Week 25 & 26 will still rise.",fill=NULL,
       caption = paste("Source: RIVM  | Plot: @YorickB | ",Sys.Date()))+
  theme(plot.background = element_rect(fill = "#E4ECFC"),
        panel.background = element_rect(fill = "#E4ECFC", colour = "#E4ECFC"),
        panel.grid.major = element_line(colour = "#E4ECFC"),
        plot.title = element_text(hjust = 0.5,size = 20,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        axis.text = element_text(size=14,color = "black", face = "bold"),
        axis.ticks = element_line(colour = "#E4ECFC", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.3, "cm"),
        axis.title.x=element_blank())
# ggsave("data/02_EN_leeftijd_heatmap-hosp.png",width=16, height = 9)







casus.working <-RIVM_casus_landelijk



casus.working <- casus.working[casus.working$Deceased == 'Yes',]

casus.working$weekOfDead.3 <-paste0(casus.working$Week_of_death, "-1")
casus.working$weekOfDead.4 <- as.Date(casus.working$weekOfDead.3, "%Y%W-%u")-7

casus.working_ny <- casus.working
casus.working_ny$weekOfDead.4 <- as.Date(casus.working_ny$weekOfDead.3, "%Y%W-%u")

casus.working <- casus.working[!is.na(casus.working$weekOfDead.4), ]
casus.working <- casus.working[casus.working$weekOfDead.4 < "2020-12-27",]

casus.working_ny$weekOfDead.4[is.na(casus.working_ny$weekOfDead.4)] <- as.Date(0)
casus.working_ny$weekOfDead.4[casus.working_ny$weekOfDead.4 == "1970-01-01"] <- "2021-01-05"

casus.working_ny <- filter(casus.working_ny[(casus.working_ny$weekOfDead.4 > "2020-12-29"),])
casus.working_ny <- filter(casus.working_ny[(casus.working_ny$weekOfDead.3 != "NA-1"),])

casus.working_ny$weekOfDead.4[casus.working_ny$weekOfDead.4 == "2021-01-05"] <- "2020-12-28"

df_today_bind <- bind_rows(casus.working_ny,casus.working)

#df_yesterday_2 <- table(df_today_bind$weekOfDead.4)
#df_yesterday_3 <-as.data.frame(df_yesterday_2)
#casus.working$weekOfDead <-paste0(casus.working$Week_of_death, "-1")
#casus.working$weekOfDead.2 <- as.Date(casus.working$weekOfDead, "%Y%W-%u")
#casus.working$weekbegin <- floor_date(casus.working$weekOfDead.2, "week", week_start = 1)

#casus.working$weekbegin <- floor_date(casus.working$date, "week", week_start = 1)
#casus.working <- casus.working[casus.working$Deceased == 'Yes',]
#casus.working$weekOfDead<-substr(casus.working$weekbegin,5,6)

df_today_bind<-count(df_today_bind,weekOfDead.4,Agegroup)
#df_today_bind <- df_today_bind[complete.cases(df_today_bind), ]  #remove N/A
df_today_bind<- df_today_bind[df_today_bind$weekOfDead.4>"2020-07-01",]



#Heatmap
ggplot(df_today_bind,aes(weekOfDead.4,Agegroup,fill=n))+
  geom_tile(size=1.5,color="#FDE3E3")+
  geom_text(label=df_today_bind$n,size=5, angle=90)+
  
  scale_fill_gradient2(trans="sqrt",low = "#5B9BD5",mid="#FFEB84",midpoint = 9, 
                       high = "#c00000")+
  scale_x_date(as.Date("2020-07-06"),breaks = "1 week",  labels = date_format("%V"))+
  coord_cartesian(expand = FALSE)+
  
  ggtitle("Overleden aan COVID-19")+
  theme_minimal()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")+
  labs(title = "Overleden aan COVID-19",
       subtitle = "Absolute getallen, binnen de leeftijdsgroep. Week 25 & 26 kunnen nog sterk stijgen",fill=NULL,
       caption = paste("Bron data: RIVM | Plot: @YorickB | ",Sys.Date()))+
  theme(plot.background = element_rect(fill = "#FDE3E3"),
        panel.background = element_rect(fill = "#FDE3E3", colour = "#FDE3E3"),
        plot.title = element_text(hjust = 0.5,size = 20,face = "bold"),
        panel.grid.major = element_line(colour = "#FDE3E3"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        axis.text = element_text(size=14,color = "black",face = "bold"),
        axis.ticks = element_line(colour = "#FDE3E3", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.3, "cm"),
        axis.title.x=element_blank())

ggsave("data/02_leeftijd_heatmap-dead.png",width=16, height = 9)



ggplot(df_today_bind,aes(weekOfDead.4,Agegroup,fill=n))+
  geom_tile(size=1.5,color="#FDE3E3")+
  geom_text(label=df_today_bind$n,size=5, angle=90)+
  scale_fill_gradient2(trans="sqrt",low = "#5B9BD5",mid="#FFEB84",midpoint = 9, 
                       high = "#c00000")+
  scale_x_date(as.Date("2020-07-06"),breaks = "1 week",  labels = date_format("%V"))+
  coord_cartesian(expand = FALSE)+
  
  ggtitle("Overleden aan COVID-19")+
  theme_minimal()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")+
  labs(title = "Deceased COVID-19",
       subtitle = "Number of deaths, within each agegroup. Week 25 & 26 will still rise.",fill=NULL,
       caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+
  theme(plot.background = element_rect(fill = "#FDE3E3"),
        panel.background = element_rect(fill = "#FDE3E3", colour = "#FDE3E3"),
        panel.grid.major = element_line(colour = "#FDE3E3"),
        plot.title = element_text(hjust = 0.5,size = 20,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        axis.text = element_text(size=14,color = "black",face = "bold"),
        axis.ticks = element_line(colour = "#FDE3E3", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.3, "cm"),
        axis.title.x=element_blank()
        )
# ggsave("data/02_EN_leeftijd_heatmap-dead.png",width=16, height = 9)















