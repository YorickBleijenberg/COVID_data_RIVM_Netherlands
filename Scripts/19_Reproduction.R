library(tidyverse)
library(jsonlite)
require(data.table)
library(lubridate)


#### reproduction

repro.name <- "https://data.rivm.nl/covid-19/COVID-19_reproductiegetal.json" #C:\\Rdir\\Mobility\\rivm-data\\2020-10-29_COVID-19_reproductiegetal.json"
reproduction.raw <- fromJSON(repro.name)
#reproduction.raw.old <- reproduction.raw.old[order(reproduction.raw.old$Type, reproduction.raw.old$Datum),]
last.date <- tail(reproduction.raw$Date, 1)
today <- Sys.Date()
# last.date <- format(Sys.time(), "%Y-%m-%d")
File_date_6 <- paste0("rivm-data/reproduction/",today,"_COVID-19_reproductiegetal.csv")
write.csv2(reproduction.raw, File_date_6, row.names=FALSE) 



last.date.old.wide.2 <- reproduction.raw
last.date.old.wide.2$Date <- as.Date(last.date.old.wide.2$Date)
last.date.old.2 <- tail(last.date.old.wide.2$Date, 1)



#### Reproduction latest ####

persco.df=data.frame(date=as.Date(c("2020-09-19", "2020-09-29", "2020-10-14",
                                    "2020-11-04", "2020-11-18", "2020-12-14",
                                    "2021-01-23", "2021-02-08", "2021-03-03",
                                    "2021-03-31", "2021-04-28", "2021-05-11",
                                    "2021-06-05", "2021-06-26", "2021-07-09")), 
                     event=c("|Kroeg uurtje eerder dicht", "|We gaan voor\nR=0,9","|Semi-lockdown\n(0.75-0.99)",
                             "|verzwaring\nsemi-lockdown\n(0.72-0.91)", "|Einde verzwaring","| lockdown, R=0.8",
                             "| Avondklok & \nbezoekbeperking", "| Basisscholen open", "| Kappers open",
                             "| Avondklok 22u", "| Stap 1", "| stap 2",
                             "| Stap 3 - einde lockdown", "| Stap 4&5", "| Extra \n maatregelen"),
                     
                     yas=c(2, 1.6, 1.4, 1.1,1.6,1.1,1.5, 1.4, 1.3, 1.1, 1.5, 1.3,
                           1.2,1.5, 2)) 

persco.dates <- data.frame(date=as.Date(c("2020-09-19", "2020-09-29", "2020-10-14",
                                          "2020-11-04", "2020-11-18", "2020-12-14",
                                          "2021-01-25", "2021-02-08", "2021-03-03",
                                          "2021-03-31", "2021-04-28", "2021-05-11",
                                          "2021-06-05", "2021-06-26", "2021-07-09")),
                           
                           event=c("19 Sep", "29 Sep", "14 Okt",
                                   "5 Nov" , "18 Nov", "14 Dec",
                                   "23 Jan", "8 Feb" , "3 Maa" ,
                                   "31 Maa", "28 Apr", "11 May",
                                   "5 jun", "26 jun", "9 jul"))

#r.estimate.df = data.frame(date_start=as.Date(c("2020-09-19", "2020-09-29", "2020-10-14", "2020-11-04")), 
#                           date_end=as.Date(c("2020-11-17", "2020-10-14", "2020-11-17", "2020-11-17"))
#                         ) 


confidence.interval <- today-10

last.date.old.wide.2 <- last.date.old.wide.2[last.date.old.wide.2$Date < confidence.interval,]

last.date.old.wide.2$Rt_low <- as.numeric(last.date.old.wide.2$Rt_low)
last.date.old.wide.2$Rt_avg <- as.numeric(last.date.old.wide.2$Rt_avg)
last.date.old.wide.2$Rt_up <- as.numeric(last.date.old.wide.2$Rt_up)


#### plot

ggplot(last.date.old.wide.2, aes(x=Date, y=Rt_avg, group = 1))+
  
  geom_vline(data=persco.df, mapping=aes(xintercept=date), color="black", linetype = "dotted")+
  
  theme_classic() + 
  xlab("")+ 
  ylab("")+
  
  scale_x_date(limits=as.Date(c("2020-9-1", today)), date_breaks = "1 months", date_labels = "%b") +
  scale_y_continuous(expand = c(0, 0), limits = c(0.5, 3.5))+ #   , breaks = c(0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5)) +
  
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()
        )+
  
  labs(x = "Datum",
       y = "Reproductiegetal",
       color = "Legend") +
  
  labs(title = "Reproductiegetal, RIVM model 28 september",
       #subtitle = "Met de beleidsdoelen voor de R", #  OMT: 'Een lagere R is beter'",
       caption = paste("Bron: RIVM | Plot: @YorickB ",Sys.Date()))+
  
  theme( plot.background = element_rect(fill = "#F5F5F5"),
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=16),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
         axis.line = element_line(colour = "#F5F5F5"),
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
  )+
  
  geom_hline(yintercept=1)+
      
#   annotate("rect", xmin = as.Date("2020-09-29"), xmax = as.Date("2020-10-14"), ymin = 0.95, ymax = 0.85, fill = "red", alpha = 0.3)+
#  annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2020-11-04"), ymin = 0.75, ymax = 0.99, fill = "blue", alpha = 0.2)+
#  annotate("rect", xmin = as.Date("2020-11-04"), xmax = as.Date("2020-11-18"), ymin = 0.72, ymax = 0.91, fill = "green", alpha = 0.3)+
#  annotate("rect", xmin = as.Date("2020-11-18"), xmax = as.Date("2020-12-14"), ymin = 0.75, ymax = 0.99, fill = "blue", alpha = 0.2)+
#  annotate("rect", xmin = as.Date("2020-12-14"), xmax = as.Date("2021-03-12"), ymin = 0.75, ymax = 0.85, fill = "red", alpha = 0.3)+
  
  geom_text(data=persco.df, mapping=aes(x=date, y=yas, label=event), size=5, angle=0, vjust=-0.4, hjust=0.012, face="bold")+
  
#  geom_text(mapping=aes(x= as.Date("2020-10-15"), y=0.6, label="Het 'hamerpakket'"), size=6, angle=0, vjust=-0.4, hjust=0.012)+
  
  geom_text(data=persco.dates, mapping=aes(x=date, y=0.55, label=event), size=3.5, angle=0, vjust=0, hjust=0.012)+
  
  geom_line(aes(y = Rt_low), lwd=0.6) +
  geom_line(aes(y = Rt_up), lwd=0.6) +
  geom_ribbon(aes(ymin=Rt_low,ymax=Rt_up), fill="darkred", alpha = 0.2) +
  geom_line(aes(y = Rt_avg, color = "Effectieve R"), color = "black",lwd=2) +


ggsave("data/plots/40_reproduction.png",width=16, height = 9)





