library(tidyverse)

today <- Sys.Date()


inwo_gem_bible <- "C:\\Rdir\\data-contstant\\biblebeltURK.csv"
gemeente.inwoners.bible <- read.csv(inwo_gem_bible,sep=";")
colnames(gemeente.inwoners.bible) = c("Municipality_code", "Gemeente_Naam", "inwoners", "gemeente_getal", "bible", "bible_inw")
gemeente.inwoners.bible <- gemeente.inwoners.bible[ -c(2:4)]

inwo_gem <- "C:\\Rdir\\data-contstant\\CBS_inwoners_gemeente.csv"
gemeente.inwoners <- read.csv(inwo_gem,sep=";")  
colnames(gemeente.inwoners) = c("Municipality_code", "Gemeente_Naam", "inwoners", "gemeente_getal")

read.aantal.gemeente.path <- paste("C:\\Rdir\\data\\",Sys.Date(),"\\", Sys.Date(), "_COVID-19_aantallen_gemeente_per_dag.csv",sep="")
RIVM_aantallen_gemeente_per_dag <- read.csv(read.aantal.gemeente.path,sep=";")

RIVM_aantallen_gemeente_per_dag$date <- as.Date(RIVM_aantallen_gemeente_per_dag$date)
RIVM_aantallen_gemeente_per_dag.1  <- RIVM_aantallen_gemeente_per_dag[ -c(1,2,4:9)]


gemeente.combi <- merge(gemeente.inwoners,gemeente.inwoners.bible, by = "Municipality_code", all.x = TRUE)

gemeente.combi$bible[is.na(gemeente.combi$bible)] <- "not_bible"
# 1556260
#17407758
#15851498

#1056266
#16.351.492
gemeente.combi$bible_inw[is.na(gemeente.combi$bible_inw)] <- 16351492 # 15851498



RIVM_aantallen_gemeente_per_dag.combi.bible<- merge(RIVM_aantallen_gemeente_per_dag.1,  gemeente.combi)

RIVM_aantallen_gemeente_per_dag.combi.bible$phd <- (RIVM_aantallen_gemeente_per_dag.combi.bible$Total_reported/RIVM_aantallen_gemeente_per_dag.combi.bible$bible_inw)*100000
#RIVM_aantallen_gemeente_per_dag.combi.bible$phd <- (RIVM_aantallen_gemeente_per_dag.combi.bible$Hospital_admission/RIVM_aantallen_gemeente_per_dag.combi.bible$bible_inw)*100000
#RIVM_aantallen_gemeente_per_dag.combi.bible$phd <- (RIVM_aantallen_gemeente_per_dag.combi.bible$Deceased/RIVM_aantallen_gemeente_per_dag.combi.bible$bible_inw)*100000




combi.3 <- aggregate(RIVM_aantallen_gemeente_per_dag.combi.bible$phd,     by=list(date=RIVM_aantallen_gemeente_per_dag.combi.bible$date,
                                                                            gemeente_Naam=RIVM_aantallen_gemeente_per_dag.combi.bible$bible), 
                     FUN=sum)
colnames(combi.3) = c("date", "bible","phd")

combi.3 <- combi.3[combi.3$date>"2020-09-15",]

#combi.3$bible <- as.factor(combi.3$bible)


combi.33 <- combi.3 %>% 
  group_by(bible) %>% 
  mutate(MAphd = rollapply(phd, 21, mean, fill = NA, align = "right"))

#combi.3 <- combi.3[combi.3$date>"2020-09-01",]

combi.33 <- combi.33[combi.33$date>"2020-09-22",]


ggplot(combi.33)+
  geom_line(aes(x=date, y=MAphd, color=bible), size = 3)+
     
  scale_fill_discrete(name = "Dose", labels = c("A", "C"))+
  
  theme_classic()+
  xlab("")+ 
  
  ylab("")+
  
  theme(legend.position = "top",
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
 
   labs(title = "De biblebelt grafiek",
       subtitle = "met 7 daags voortschrijdend gemiddelde",
       caption = paste("Bron: RIVM/wikipedia | Plot: @YorickB | ",Sys.Date()))+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    #scale_x_date(),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    #axis.text.x=element_blank(),
    axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
ggsave("data/75_Municipality-bible-ma-URK-21.png",width=16, height = 09)



RIVM_aantallen_gemeente_per_dag.combi.bible$phd <- (RIVM_aantallen_gemeente_per_dag.combi.bible$Hospital_admission/RIVM_aantallen_gemeente_per_dag.combi.bible$bible_inw)*100000


combi.3 <- aggregate(RIVM_aantallen_gemeente_per_dag.combi.bible$phd,     by=list(date=RIVM_aantallen_gemeente_per_dag.combi.bible$date,
                                                                                  gemeente_Naam=RIVM_aantallen_gemeente_per_dag.combi.bible$bible), 
                     FUN=sum)
colnames(combi.3) = c("date", "bible","phd")

combi.3 <- combi.3[combi.3$date>"2020-09-15",]

#combi.3$bible <- as.factor(combi.3$bible)


combi.33 <- combi.3 %>% 
  group_by(bible) %>% 
  mutate(MAphd = rollapply(phd, 21, mean, fill = NA, align = "right"))

#combi.3 <- combi.3[combi.3$date>"2020-09-01",]

combi.33 <- combi.33[combi.33$date>"2020-09-22",]


ggplot(combi.33)+
  geom_line(aes(x=date, y=MAphd, color=bible), size = 3)+
  
  scale_fill_discrete(name = "Dose", labels = c("A", "C"))+
  
  theme_classic()+
  xlab("")+ 
  
  ylab("")+
  
  theme(legend.position = "top",
        legend.background = element_rect(fill="#E4ECFC",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
  
  labs(title = "De biblebelt grafiek - ziekenhuisopnames",
       subtitle = "met 7 daags voortschrijdend gemiddelde",
       caption = paste("Bron: RIVM/wikipedia | Plot: @YorickB | ",Sys.Date()))+
  
  theme(
    plot.background = element_rect(fill = "#E4ECFC"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#E4ECFC", colour = "#E4ECFC"),
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    #scale_x_date(),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    #axis.text.x=element_blank(),
    axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
    axis.ticks = element_line(colour = "#E4ECFC", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#E4ECFC"),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
  ggsave("data/75_Municipality-bible-ma-URK-21-hosp.png",width=16, height = 09)




RIVM_aantallen_gemeente_per_dag.combi.bible$phd <- (RIVM_aantallen_gemeente_per_dag.combi.bible$Deceased/RIVM_aantallen_gemeente_per_dag.combi.bible$bible_inw)*100000
combi.3 <- aggregate(RIVM_aantallen_gemeente_per_dag.combi.bible$phd,     by=list(date=RIVM_aantallen_gemeente_per_dag.combi.bible$date,
                                                                                  gemeente_Naam=RIVM_aantallen_gemeente_per_dag.combi.bible$bible), 
                     FUN=sum)
colnames(combi.3) = c("date", "bible","phd")

combi.3 <- combi.3[combi.3$date>"2020-09-15",]

#combi.3$bible <- as.factor(combi.3$bible)


combi.33 <- combi.3 %>% 
  group_by(bible) %>% 
  mutate(MAphd = rollapply(phd, 21, mean, fill = NA, align = "right"))

#combi.3 <- combi.3[combi.3$date>"2020-09-01",]

combi.33 <- combi.33[combi.33$date>"2020-09-22",]


ggplot(combi.33)+
  geom_line(aes(x=date, y=MAphd, color=bible), size = 3)+
  
  scale_fill_discrete(name = "Dose", labels = c("A", "C"))+
  
  theme_classic()+
  xlab("")+ 
  
  ylab("")+
  
  theme(legend.position = "top",
        legend.background = element_rect(fill="#FDE3E3",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
  
  labs(title = "De biblebelt grafiek - doden",
       subtitle = "met 7 daags voortschrijdend gemiddelde",
       caption = paste("Bron: RIVM/wikipedia | Plot: @YorickB | ",Sys.Date()))+
  
  theme(
    plot.background = element_rect(fill = "#FDE3E3"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#FDE3E3", colour = "#FDE3E3"),
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    #scale_x_date(),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    #axis.text.x=element_blank(),
    axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
    axis.ticks = element_line(colour = "#FDE3E3", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#FDE3E3"),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
  ggsave("data/75_Municipality-bible-ma-URK-21-dead.png",width=16, height = 09)








