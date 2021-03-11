library(tidyverse)

vr <- "C:\\Rdir\\data-contstant\\veiligheidsregios.csv"
VR <- read.csv(vr,sep=";")  
colnames(VR) = c("vrcode", "Regio_Naam", "inwoners", "regio_vac", "Municipal_health_service")

FR_a <- intToUtf8(0x00E2)  #Encoding(FR_b) <- "UTF-8"
FR_b <- paste0("Frysl", FR_a,"n")
FR_c <- paste0("GGD Frysl", FR_a,"n")
VR$Regio_Naam <- str_replace(VR$Regio_Naam, "Fryslân", FR_b)  ##fout / goed
VR$Municipal_health_service <- str_replace(VR$Municipal_health_service, "GGD Fryslân", FR_c)  ##fout / goed

#VR$GGD_naam <- as.factor(VR$GGD_naam)
#VR$inwoners <- as.integer(VR$inwoners)

##  read.aantal.landelijk.path <- paste("C:\\Rdir\\data\\",Sys.Date()-1,"\\", Sys.Date()-1, "_COVID-19_casus_landelijk.csv",sep="")
##  cases_per_yesterday <- read.csv(read.aantal.landelijk.path,sep=";")

read.aantal.landelijk.path <- paste("C:\\Rdir\\data\\",Sys.Date(),"\\", Sys.Date(), "_COVID-19_casus_landelijk.csv",sep="")
cases_per_day <- read.csv(read.aantal.landelijk.path,sep=";")

cases_per_day$date  <- as.Date(cases_per_day$date)

FR_a <- intToUtf8(0x00E2)  #Encoding(FR_b) <- "UTF-8"
FR_b <- paste0("Frysl", FR_a,"n")
cases_per_day$Municipal_health_service <- str_replace(cases_per_day$Municipal_health_service, "Fryslân", FR_b)  ##fout / goed

cases_per_day.2 <- cases_per_day[ -c(1,2,4:9,12)]

cases_per_day.3 <- merge(VR, cases_per_day.2)

#pop.niet.noord = 11229472
#pop.noord =   6198334


cases_per_day.3$value <- 1
#cases_per_day_long <- aggregate(cases_per_day$value, by = list(Type_Datum = cases_per_day$Date_statistics_type, Datum = cases_per_day$date, Dag = cases_per_day$Date_file), FUN = sum)
#colnames(cases_per_day_long)[4] <- "value"


cases_per_day.3 <- cases_per_day.3[cases_per_day.3$date>"2020-09-01",]
cases_per_day.3$Date_statistics_type <- as.factor(cases_per_day.3$Date_statistics_type) 

VR$Regio_Naam <- str_replace(VR$Regio_Naam, "Fryslân", FR_b)  ##fout / goed
VR$Municipal_health_service <- str_replace(VR$Municipal_health_service, "GGD Fryslân", FR_c)  ##fout / goed

rect2 <- data.frame(xmin = c(as.Date("2020-10-10"), as.Date("2020-10-17")),
                    xmax = c(as.Date("2020-10-18"), as.Date("2020-10-25")),
                    ymin = c(0, 0),
                    ymax = c(Inf, Inf),
                    regio_vac = c("Noord", "Niet-Noord")
)

ggplot()+
  geom_col(data= cases_per_day.3, aes(x=date , y = value, fill = factor(Date_statistics_type, levels=c("DOO","DPL","DON") )))+
  
  scale_fill_manual(values=c("#00ba38", "#203864", "#f8766d"),  #"red","blue"),   #"#f8766d", "#203864", "#00ba38"
                    labels=c(   "Eerste ziektedag",
                                "Positieve labuitslag",
                                "Melding aan GGD") #,
                                #"Vakantie Niet-Noord",
                                #"Vakantie Noord")
                    )+
  
  #geom_rect(data = rect2 , aes(xmin = xmin,
  #                             xmax = xmax,
  #                            ymin = ymin,
  #                            ymax = ymax,
  #                            fill = regio_vac),
  #        alpha = 0.5) +
  
  
  facet_wrap(~Regio_Naam ,  scales = "free_y")+
  
  theme_bw() + 
  xlab("")+ 
  ylab("")+
  scale_x_date()+
  
  theme(legend.position = "top",
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
  
  labs(title = "casus data per veiligheidsregio",
     #  subtitle = paste("Herfstvakantie Noord: 10-18 oktober \n",
      #                  "semi-lockdown op 14 oktober\n",
      #                  "Herfstvakantie midden/zuid: 17-25 oktober"),
       caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=12),  #, angle=45),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
         axis.line = element_line(colour = "black"),
         
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
         ### facet label custom
         strip.text.x = element_text(size = 13, color = "black"),
         strip.background = element_rect(color="black", fill="gray", size=1.5, linetype="solid"),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank()
  )
 # geom_vline(xintercept = as.Date("2020-10-14"), linetype = "dashed", color = "black", size = 1.5)

ggsave("data/70_Security_region-casus.png",width=16, height = 16)











read.aantal.gemeente.path <- paste("C:\\Rdir\\data\\",Sys.Date(),"\\", Sys.Date(), "_COVID-19_aantallen_gemeente_per_dag.csv",sep="")
RIVM_aantallen_vr_per_dag.2 <- read.csv(read.aantal.gemeente.path,sep=";")

RIVM_aantallen_vr_per_dag.2$date <- as.Date(RIVM_aantallen_vr_per_dag.2$date)
RIVM_aantallen_vr_per_dag.3  <- RIVM_aantallen_vr_per_dag.2[ -c(1,2,3,4,5,9)]

today <- Sys.Date()


RIVM_aantallen_vr_per_dag.3 <- aggregate(RIVM_aantallen_vr_per_dag.2$Total_reported,     by=list(dateInTable=RIVM_aantallen_vr_per_dag.2$date,
                                                                                                             secure=RIVM_aantallen_vr_per_dag.2$Security_region_name), 
                                               FUN=sum)
colnames(RIVM_aantallen_vr_per_dag.3) = c("date", "Security_region_name", "Total_reported")

RIVM_aantallen_vr_per_dag.3$Security_region_name <- str_replace(RIVM_aantallen_vr_per_dag.3$Security_region_name, "GGD Fryslân", FR_b)  ##fout / goed
RIVM_aantallen_vr_per_dag.3 <- subset(RIVM_aantallen_vr_per_dag.3, Security_region_name !='')

#RIVM_aantallen_gemeente_per_dag.3 <- subset(RIVM_aantallen_gemeente_per_dag.3, Security_region_name == 'Drenthe')

RIVM_aantallen_vr_per_dag.3 <- RIVM_aantallen_vr_per_dag.3[RIVM_aantallen_vr_per_dag.3$date>"2020-09-01",]


ggplot(data= RIVM_aantallen_vr_per_dag.3, aes(x=date , y = Total_reported))+
  geom_bar(stat='identity',  fill = "#96afde")+
 geom_line()+
 geom_point(size = 2)+
  
facet_wrap(~Security_region_name,  scales = "free_y")+
  scale_x_date(date_breaks = "1 day", 
             date_labels= format("%d %b"),
           limits = as.Date(c("2020-11-12", today)))+
   theme_classic()

ggsave("data/71_Security_region-perdag.png",width=16, height = 16)

