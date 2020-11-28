#library(tidyverse)
#library(jsonlite)
require(data.table)

### Herstvakantie-effect ###

repro.inno <- "https://raw.githubusercontent.com/innouveau/corona-map/master/public/data/maps/nederland/cities.json" #C:\\Rdir\\Mobility\\rivm-data\\2020-10-29_COVID-19_reproductiegetal.json"
cities.inno.raw <- fromJSON(repro.inno)

cities.inno.raw_short <- cities.inno.raw[,c("title", "population","regio_title")]
colnames(cities.inno.raw_short) <- c("gemeente","population", "regio")

cities.inno.raw_short_pop  <- aggregate(cities.inno.raw_short$population,  by=list(Category=cities.inno.raw_short$regio), FUN=sum)
colnames(cities.inno.raw_short_pop) <- c("regio", "population")


pop.niet.noord = cities.inno.raw_short_pop$x[1]   # 11229472
pop.noord = cities.inno.raw_short_pop$x[2]        #  6198334

######copy.aantal.gem.dag.2   <-   aantallen_gemeente_per_dag
read.aantal.path <-paste("C:\\Rdir\\data\\",Sys.Date(),"\\", Sys.Date(), "_COVID-19_aantallen_gemeente_per_dag.csv", sep = "")
copy.aantal.gem.dag.2 <- read.csv(read.aantal.path,sep=";")

copy.aantal.gem.dag.short <- copy.aantal.gem.dag.2[,c("Date_of_publication", "Municipality_name","Total_reported")]
copy.aantal.gem.dag.short = filter(copy.aantal.gem.dag.short, Municipality_name != "")
colnames(copy.aantal.gem.dag.short) <- c("datum","gemeente", "cases")


noord.zuid <- merge(copy.aantal.gem.dag.short,cities.inno.raw_short)
noord.zuid$datum <- as.Date(noord.zuid$datum)

noord.zuid.short.2 <- aggregate(noord.zuid$cases, by=list(noord.zuid$regio, noord.zuid$datum), FUN=sum)

#noord.zuid.short$posrate <- round(noord.zuid.short$cases/noord.zuid.short$population*100000, digits = 2)

colnames(noord.zuid.short.2) <- c("regio","datum", "cases")

noord.zuid.short.3 <-merge(noord.zuid.short.2, cities.inno.raw_short_pop)
noord.zuid.short.3$posrate <- round(noord.zuid.short.3$cases/noord.zuid.short.3$population*100000, digits = 2)

#noord.zuid.short.2$posrate <- round(noord.zuid.short.2$cases/noord.zuid.short.2$population*100000, digits = 2)


noord.zuid.short.4 <- noord.zuid.short.3[order(noord.zuid.short.3$regio, noord.zuid.short.3$datum),]

noord.zuid.short.4$MA_posrate <- round(rollmeanr(noord.zuid.short.4$posrate, 7, fill = NA),digits = 1)

noord.zuid.short.4 <- noord.zuid.short.4[-1:-6,]

noord.zuid.short.4 <-filter(noord.zuid.short.4, datum > "2020-09-15")
  

#### vanaties | regio plot ####

ggplot(noord.zuid.short.4, aes(x=datum, y = posrate))+
  
  geom_bar(stat='identity', fill = "#96afde")+
  geom_line(mapping = aes(x=datum, y=MA_posrate), color = "#F5F5F5",lwd = 3)+
  geom_line(mapping = aes(x=datum, y=MA_posrate), color = "#44546a",lwd = 2)+
  
  facet_wrap(~regio)+
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  scale_x_date(name="")+
  scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","))+
  labs(title = "Vakanties",
       subtitle = paste("Per 100.000  ||  7-daags zwevend gemiddele",
                        "\n\n - rood - Herfstvakantie Noord: 10-18 oktober \n",
                        "- zwart = semi-lockdown op 14 oktober\n",
                        "- blauw - Herfstvakantie midden/zuid: 17-25 oktober"
                                                ),
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
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
  
  # geom_vline(xintercept = as.Date("2020-10-10"), linetype = "dotted", color = "darkgreen",size = 1.5) +
  # geom_vline(xintercept = as.Date("2020-10-18"), linetype = "dotted", color = "darkgreen",size = 1.5) +
  # geom_vline(xintercept = as.Date("2020-10-17"), linetype = "dotted", size = 1.5) +
  # geom_vline(xintercept = as.Date("2020-10-25"), linetype = "dotted", size = 1.5)+
  
  annotate("rect", xmin = as.Date("2020-10-10"), xmax = as.Date("2020-10-18"), ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.2)+
  annotate("rect", xmin = as.Date("2020-10-17"), xmax = as.Date("2020-10-25"), ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.2)+
  geom_vline(xintercept = as.Date("2020-10-14"), linetype = "dashed", color = "black", size = 1.5)+
 
ggsave("data/40_niet-noord-phd.png",width=16, height = 9)          


#### vanaties | regio plot EN ####

ggplot(noord.zuid.short.4, aes(x=datum, y = posrate))+
  geom_bar(stat='identity', fill = "#96afde")+
  geom_line(mapping = aes(x=datum, y=MA_posrate), color = "#F5F5F5",lwd = 3)+
  geom_line(mapping = aes(x=datum, y=MA_posrate), color = "#44546a",lwd = 2)+
  
  facet_wrap(~regio)+
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  scale_x_date(name="")+
  scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","))+
  labs(title = "Vacations",
       subtitle = paste("Per 100.000  ||  7-day moving average",
                        "\n\n - green - Fall vacation region 'North': 10-18 october \n",
                        "- red = semi-lockdown on 14 october\n",
                        "- black - Fall vacation 'Middle / South': 17-25 october"
       ),
       caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
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
  
  geom_vline(xintercept = as.Date("2020-10-10"), linetype = "dotted", color = "darkgreen",size = 1.5) +
  geom_vline(xintercept = as.Date("2020-10-18"), linetype = "dotted", color = "darkgreen",size = 1.5) +
  geom_vline(xintercept = as.Date("2020-10-17"), linetype = "dotted", size = 1.5) +
  geom_vline(xintercept = as.Date("2020-10-25"), linetype = "dotted", size = 1.5)+
  
  geom_vline(xintercept = as.Date("2020-10-14"), linetype = "dashed", color = "red", size = 1.5)+
  
  ggsave("data/40_EN_niet-noord-phd.png",width=16, height = 9)    










vr <- "C:\\Rdir\\data-contstant\\veiligheidsregios.csv"
VR <- read.csv(vr,sep=";")  
colnames(VR) = c("vrcode", "Regio_Naam", "inwoners", "regio_vac", "Municipal_health_service")

FR_a <- intToUtf8(0x00E2)  #Encoding(FR_b) <- "UTF-8"
FR_b <- paste0("Frysl", FR_a,"n")
FR_c <- paste0("GGD Frysl", FR_a,"n")
VR$Regio_Naam <- str_replace(VR$Regio_Naam, "Fryslân", FR_b)  ##fout / goed
VR$Municipal_health_service <- str_replace(VR$Municipal_health_service, "GGD Fryslân", FR_c)  ##fout / goed



read.aantal.landelijk.path <- paste("C:\\Rdir\\data\\",Sys.Date(),"\\", Sys.Date(), "_COVID-19_casus_landelijk.csv",sep="")
cases_per_day <- read.csv(read.aantal.landelijk.path,sep=";")

cases_per_day$date  <- as.Date(cases_per_day$date)

FR_a <- intToUtf8(0x00E2)  #Encoding(FR_b) <- "UTF-8"
FR_b <- paste0("Frysl", FR_a,"n")
cases_per_day$Municipal_health_service <- str_replace(cases_per_day$Municipal_health_service, "Fryslân", FR_b)  ##fout / goed

cases_per_day.2 <- cases_per_day[ -c(1,2,4:9,12)]

cases_per_day.3 <- merge(VR, cases_per_day.2)

cases_per_day.3 <- cases_per_day.3[cases_per_day.3$date>"2020-09-01",]
cases_per_day.3$Date_statistics_type <- as.factor(cases_per_day.3$Date_statistics_type) 

cases_per_day.3$value <- 1

rect2 <- data.frame(xmin = c(as.Date("2020-10-10"), as.Date("2020-10-17")),
                    xmax = c(as.Date("2020-10-18"), as.Date("2020-10-25")),
                    ymin = c(0, 0),
                    ymax = c(Inf, Inf),
                    regio_vac = c("Noord", "Niet-Noord")
)

ggplot()+
  geom_col(data= cases_per_day.3, aes(x=date , y = value, fill = factor(Date_statistics_type, levels=c("DOO","DPL","DON") )))+
  
  scale_fill_manual(values=c("#f8766d", "#00ba38", "#203864","red","blue"),
                    labels=c(   "Melding aan GGD",
                                "Eerste ziektedag",
                                "Positieve labuitslag",
                                "Vakantie Niet-Noord",
                                "Vakantie Noord"))+
  scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","))+
  geom_rect(data = rect2 , aes(xmin = xmin,
                               xmax = xmax,
                               ymin = ymin,
                               ymax = ymax,
                               fill = regio_vac),
            alpha = 0.5) +

  facet_wrap(~regio_vac)+ 
  
  theme_bw() + 
  xlab("")+ 
  ylab("")+
  scale_x_date()+
  
  theme(legend.position = "top",
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
  
  labs(title = "Vakanties - casus data",
       subtitle = paste("Herfstvakantie Noord: 10-18 oktober \n",
                        "semi-lockdown op 14 oktober\n",
                        "Herfstvakantie midden/zuid: 17-25 oktober"),
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
  )+
  geom_vline(xintercept = as.Date("2020-10-14"), linetype = "dashed", color = "black", size = 1.5)

ggsave("data/40_niet-noord-raw.png",width=16, height = 9)



