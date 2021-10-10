library(tidyverse)

today <- Sys.Date()


inwo_gem_vax <- "C:\\Rdir\\data-contstant\\vacc_level_cities.csv"
gemeente.inwoners.vax_level <- read.csv(inwo_gem_vax,sep=";")
colnames(gemeente.inwoners.vax_level) = c("Municipality_code", "Gemeente_Naam", "inwoners", "gemeente_getal", "vax_level", "vax_lvl_inw","a","b","c","vax_graad")
gemeente.inwoners.vax_level <- gemeente.inwoners.vax_level[ -c(2,4,7:9)]


read.aantal.gemeente.path <- paste("C:\\Rdir\\data\\",Sys.Date(),"\\", Sys.Date(), "_COVID-19_aantallen_gemeente_per_dag.csv",sep="")
# RIVM_aantallen_gemeente_per_dag <- read.csv(read.aantal.gemeente.path,sep=";")
  RIVM_aantallen_gemeente_per_dag<-read.csv("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv",sep=";")  #import from RIVM website

RIVM_aantallen_gemeente_per_dag$date <- as.Date(RIVM_aantallen_gemeente_per_dag$Date_of_publication)
RIVM_aantallen_gemeente_per_dag.vax  <- RIVM_aantallen_gemeente_per_dag[ -c(1,2,4:9)]


vax_lvl.daily.numbers<- merge(RIVM_aantallen_gemeente_per_dag.vax,  gemeente.inwoners.vax_level)

vax_lvl.daily.numbers$cases.phd <- (vax_lvl.daily.numbers$Total_reported      /vax_lvl.daily.numbers$vax_lvl_inw)*100000
vax_lvl.daily.numbers$hosp.phd  <- (vax_lvl.daily.numbers$Hospital_admission  /vax_lvl.daily.numbers$vax_lvl_inw)*100000
vax_lvl.daily.numbers$death.phd <- (vax_lvl.daily.numbers$Deceased            /vax_lvl.daily.numbers$vax_lvl_inw)*100000

#### cases #### 
vax_lvl.daily.numbers.cases <- aggregate(vax_lvl.daily.numbers$cases.phd,     by=list(date           = vax_lvl.daily.numbers$date,
                                                                                      gemeente_Naam  = vax_lvl.daily.numbers$vax_level),FUN=sum)
colnames(vax_lvl.daily.numbers.cases) = c("date", "vax_level","phd")
vax_lvl.daily.numbers.cases <- vax_lvl.daily.numbers.cases[vax_lvl.daily.numbers.cases$date>"2020-10-15",]

vax_lvl.daily.numbers.cases <- vax_lvl.daily.numbers.cases %>%  group_by(vax_level) %>% mutate(MAphd = rollapply(phd, 7, mean, fill = NA, align = "right"))

#### hosp #### 
vax_lvl.daily.numbers.hosp <- aggregate(vax_lvl.daily.numbers$hosp.phd,     by=list(date           = vax_lvl.daily.numbers$date,
                                                                                    gemeente_Naam  = vax_lvl.daily.numbers$vax_level),FUN=sum)
colnames(vax_lvl.daily.numbers.hosp) = c("date", "vax_level","phd")
vax_lvl.daily.numbers.hosp <- vax_lvl.daily.numbers.hosp[vax_lvl.daily.numbers.hosp$date>"2020-10-15",]
vax_lvl.daily.numbers.hosp <- vax_lvl.daily.numbers.hosp %>% group_by(vax_level) %>% mutate(MAphd = rollapply(phd, 7, mean, fill = NA, align = "right"))

#### dead #### 
vax_lvl.daily.numbers.dead <- aggregate(vax_lvl.daily.numbers$death.phd,     by=list(date           = vax_lvl.daily.numbers$date,
                                                                                     gemeente_Naam  = vax_lvl.daily.numbers$vax_level),FUN=sum)
colnames(vax_lvl.daily.numbers.dead) = c("date", "vax_level","phd")
vax_lvl.daily.numbers.dead <- vax_lvl.daily.numbers.dead[vax_lvl.daily.numbers.dead$date>"2020-10-15",]
vax_lvl.daily.numbers.dead <- vax_lvl.daily.numbers.dead %>% group_by(vax_level) %>% mutate(MAphd = rollapply(phd, 7, mean, fill = NA, align = "right"))



vax_lvl.daily.numbers.cases.short <- vax_lvl.daily.numbers.cases[vax_lvl.daily.numbers.cases$date>"2021-08-01",]
vax_lvl.daily.numbers.hosp.short <-  vax_lvl.daily.numbers.hosp[vax_lvl.daily.numbers.hosp$date>"2021-08-01",]
vax_lvl.daily.numbers.dead.short <-  vax_lvl.daily.numbers.dead[vax_lvl.daily.numbers.dead$date>"2021-08-01",]


#### plot ####


ggplot(vax_lvl.daily.numbers.cases.short)+
  
  geom_line(aes(x=date, y=MAphd, color=factor(vax_level, levels=c("hoog", "middle", "laag"))), size = 3)+
  
  scale_color_manual(values = c("#3c81b9","#000000","#e5292b"),labels=c ("hoog (>85%)", "middel (75% - 85%)", "laag (<75%)" ))+
    theme_classic()+
  xlab("")+ 
  ylab("")+
  
  theme(legend.position = "top",
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
 
  labs(title = "RIVM besmettingen - indeling obv de huidige vaccinatiegraad",
       subtitle = "Aantal nieuwe besmettingen per 100k \n met 7 daags voortschrijdend gemiddelde",
       caption = paste("Bron: RIVM/wikipedia | Plot: @YorickB | ",Sys.Date()))+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"),
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
ggsave("data/82_Municipality_vax_level_cases.png",width=16, height = 09)







ggplot(vax_lvl.daily.numbers.hosp.short)+
  geom_line(aes(x=date, y=MAphd, color=factor(vax_level, levels=c("hoog", "middle", "laag"))), size = 3)+
  
  scale_color_manual(values = c("#3c81b9","#000000","#e5292b"),labels=c ("hoog (>85%)", "middel (75% - 85%)", "laag (<75%)" ))+
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
  theme(legend.position = "top",
        legend.background = element_rect(fill="#E4ECFC",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
  
  labs(title = "RIVM opnames - indeling obv de huidige vaccinatiegraad",
       subtitle = "Aantal nieuwe opnames per 100k \n met 7 daags voortschrijdend gemiddelde",
       caption = paste("Bron: RIVM/wikipedia | Plot: @YorickB | ",Sys.Date()))+
  
  theme(
    plot.background = element_rect(fill = "#E4ECFC"),
    panel.background = element_rect(fill = "#E4ECFC", colour = "#E4ECFC"),
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14),
    axis.ticks = element_line(colour = "#E4ECFC", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#E4ECFC"),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
  ggsave("data/82_Municipality_vax_level_hosp.png",width=16, height = 09)






ggplot(vax_lvl.daily.numbers.dead.short)+
  geom_line(aes(x=date, y=MAphd, color=factor(vax_level, levels=c("hoog", "middle", "laag"))), size = 3)+
 
  scale_color_manual(values = c("#3c81b9","#000000","#e5292b"),labels=c ("hoog (>85%)", "middel (75% - 85%)", "laag (<75%)" ))+
 
   theme_classic()+
  xlab("")+ 
  ylab("")+
  
  theme(legend.position = "top",
        legend.background = element_rect(fill="#FDE3E3",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
  
  labs(title = "RIVM doden - indeling obv de huidige vaccinatiegraad",
       subtitle = "Aantal nieuwe doden per 100k \n met 7 daags voortschrijdend gemiddelde",
       caption = paste("Bron: RIVM/wikipedia | Plot: @YorickB | ",Sys.Date()))+
  
  theme(
    plot.background = element_rect(fill = "#FDE3E3"),
    panel.background = element_rect(fill = "#FDE3E3", colour = "#FDE3E3"),
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14),
    axis.ticks = element_line(colour = "#FDE3E3", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#FDE3E3"),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
  ggsave("data/82_Municipality_vax_level_dead.png",width=16, height = 09)






