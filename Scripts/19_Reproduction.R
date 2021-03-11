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

#################### old
repro.name.old <- "C:\\Rdir\\rivm-data\\reproduction\\2021-02-02_COVID-19_reproductiegetal.csv"
reproduction.raw.old <- read.csv(repro.name.old,sep=";")  

last.date.old.wide <-reproduction.raw.old
last.date.old.wide$Date <- as.Date(last.date.old.wide$Date)
last.date.old.wide$Rt_low <- as.numeric(sub("," , ".", last.date.old.wide$Rt_low))
last.date.old.wide$Rt_avg <- as.numeric(sub("," , ".", last.date.old.wide$Rt_avg))
last.date.old.wide$Rt_up  <- as.numeric(sub("," , ".", last.date.old.wide$Rt_up))

last.date.old <- tail(last.date.old.wide$Date, 1)

#last.date.old.wide <- spread(reproduction.raw.old, key="Type", value="Waarde")
#colnames(last.date.old.wide) = c("Date","Rt_up", "Rt_low", "Rt_avg" )

#last.date.old.wide$Date <- as.Date(last.date.old.wide$Date)

####################

################### newer
#   repro.name.old.2 <- "C:\\Rdir\\rivm-data\\reproduction\\2020-10-27_COVID-19_reproductiegetal.csv"
#   reproduction.raw.old.2 <- read.csv(repro.name.old.2,sep=",")  

#   last.date.old.2 <- tail(reproduction.raw.old.2$Datum, 1)
#   reproduction.raw.old.2$Datum <- as.Date(reproduction.raw.old.2$Datum)

#   last.date.old.wide.2 <- spread(reproduction.raw.old.2, key="Type", value="Waarde")
 #  colnames(last.date.old.wide.2) = c("Date","Rt_up", "Rt_low", "Rt_avg" )


#### Reproduction latest ####

persco.df=data.frame(date=as.Date(c("2020-09-19", "2020-09-29", "2020-10-14", "2020-11-04","2020-11-18","2020-12-14", "2021-01-23", "2021-02-08")), 
                        event=c("|Kroeg uurtje eerder dicht", "|We gaan voor\nR=0,9","|Semi-lockdown\n(0.75-0.99)", "|verzwaring\nsemi-lockdown\n(0.72-0.91)", "|Einde verzwaring",
                                "| lockdown, R=0.8","| Avondklok & \nbezoekbeperking", "| Basisscholen open"),
                     yas=c(1.01, 0.85, 0.75, 0.72,0.75,0.75,0.75, 0.9)
                     )

persco.dates <- data.frame(date=as.Date(c("2020-09-19", "2020-09-29", "2020-10-14", "2020-11-04","2020-11-18", "2020-12-14", "2021-01-25", "2021-02-08")),
                                     event=c("19 sep", "29 sep", "14 okt", "5 nov","18 nov", "14 Dec", "23 Jan", "8 Feb"))

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
    
  scale_x_date(limits=as.Date(c("2020-9-1", today)), date_breaks = "2 week", date_labels = "%b %d") +
  scale_y_continuous(expand = c(0, 0), limits = c(0.5, 1.5), breaks = c(0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5)) +
  
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()
        ) +
  
  labs(x = "Datum",
       y = "Reproductiegetal",
       color = "Legend") +
  
  labs(title = "Reproductiegetal, RIVM model 9 maart",
       subtitle = "Met de beleidsdoelen voor de R", #  OMT: 'Een lagere R is beter'",
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

  annotate("rect", xmin = as.Date("2020-09-29"), xmax = as.Date("2020-10-14"), ymin = 0.95, ymax = 0.85, fill = "red", alpha = 0.3)+
  annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2020-11-04"), ymin = 0.75, ymax = 0.99, fill = "blue", alpha = 0.2)+
  annotate("rect", xmin = as.Date("2020-11-04"), xmax = as.Date("2020-11-18"), ymin = 0.72, ymax = 0.91, fill = "green", alpha = 0.3)+
  annotate("rect", xmin = as.Date("2020-11-18"), xmax = as.Date("2020-12-14"), ymin = 0.75, ymax = 0.99, fill = "blue", alpha = 0.2)+
  annotate("rect", xmin = as.Date("2020-12-14"), xmax = as.Date("2021-02-23"), ymin = 0.75, ymax = 0.85, fill = "red", alpha = 0.3)+
  
  geom_text(data=persco.df, mapping=aes(x=date, y=yas, label=event), size=6, angle=0, vjust=-0.4, hjust=0.012, face="bold")+

  geom_text(mapping=aes(x= as.Date("2020-10-15"), y=0.6, label="Het 'hamerpakket'"), size=6, angle=0, vjust=-0.4, hjust=0.012)+
  
  geom_text(data=persco.dates, mapping=aes(x=date, y=0.55, label=event), size=4, angle=0, vjust=0, hjust=0.012)+
  
  
  
  
  geom_line(aes(y = Rt_low), lwd=0.6) +
  geom_line(aes(y = Rt_up), lwd=0.6) +
  geom_ribbon(aes(ymin=Rt_low,ymax=Rt_up), fill="darkred", alpha = 0.2) +
  geom_line(aes(y = Rt_avg, color = "Effectieve R"), color = "black",lwd=2) 
  
  
ggsave("data/40_reproduction.png",width=16, height = 9)






#persco.df=data.frame(date=as.Date(c("2020-09-19", "2020-09-29", "2020-10-14", "2020-11-04","2020-11-18")), 
#                     event=c("|Kroeg uurtje eerder dicht", "|We gaan voorR=0,9","|Semi-lockdown\n(0.75-0.99)", "|verzwaring\nsemi-lockdown\n(0.72-0.91)", "|Einde verzwaring"),
#                     yas=c(1.01, 0.9, 0.75, 0.72,0.75)
#)

#r.estimate.df = data.frame(date_start=as.Date(c("2020-09-19", "2020-09-29", "2020-10-14", "2020-11-04")), 
#                           date_end=as.Date(c("2020-11-17", "2020-10-14", "2020-11-17", "2020-11-17"))
#                         ) 


ggplot(last.date.old.wide.2, aes(x=Date, y=Rt_avg, group = 1))+
  
  geom_vline(data=persco.df, mapping=aes(xintercept=date), color="black", linetype = "dotted") +
  
  
  geom_line(aes(y = Rt_low), lwd=0.6) +
  geom_line(aes(y = Rt_up), lwd=0.6) +
  geom_ribbon(aes(ymin=Rt_low,ymax=Rt_up), fill="darkred", alpha = 0.2) +
  geom_line(aes(y = Rt_avg, color = "Effectieve R"), color = "black",lwd=2) +
  
  theme_classic() + 
  xlab("")+ 
  ylab("")+
  
  scale_x_date(limits=as.Date(c("2020-9-1", today)), date_breaks = "1 week", date_labels = "%b %d") +
  scale_y_continuous(expand = c(0, 0), limits = c(0.5, 1.5)) +
  
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()
  ) +
  
  labs(x = "Datum",
       y = "Reproductiegetal",
       color = "Legend") +
  
  labs(title = "Reproductiegetal, model 9 februari",
       subtitle = "Beleidsdoelen voor de R", #  OMT: 'Een lagere R is beter'",
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
  
  #annotate("rect", xmin = as.Date("2020-09-29"), xmax = as.Date("2020-10-14"), ymin = 0.95, ymax = 0.85, fill = "red", alpha = 0.3)+
  #annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2020-11-04"), ymin = 0.75, ymax = 0.99, fill = "blue", alpha = 0.2)+
  #annotate("rect", xmin = as.Date("2020-11-04"), xmax = as.Date("2020-11-18"), ymin = 0.72, ymax = 0.91, fill = "green", alpha = 0.3)+
  #annotate("rect", xmin = as.Date("2020-11-18"), xmax = as.Date("2020-12-03"), ymin = 0.75, ymax = 0.99, fill = "blue", alpha = 0.2)+
  
  annotate("rect", xmin = as.Date("2020-10-10"), xmax = as.Date("2020-10-18"), ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.2)+
  annotate("rect", xmin = as.Date("2020-10-17"), xmax = as.Date("2020-10-25"), ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.2)+
  annotate("rect", xmin = as.Date("2020-12-19"), xmax = as.Date("2021-01-04"), ymin = -Inf, ymax = Inf, fill = "green", alpha = 0.2)+
  
  
  geom_text(data=persco.df, mapping=aes(x=date, y=yas, label=event), size=6, angle=0, vjust=-0.4, hjust=0.012, face="bold")


ggsave("data/40_reproduction-vac-2.png",width=16, height = 9)



















##### Combine plot #####

ggplot(last.date.old.wide, aes(x=Date, y=Rt_avg))+
  
  ## new prediction
  geom_line(data=last.date.old.wide.2, aes(y = Rt_low), lwd=0.6) +
  geom_line(data=last.date.old.wide.2, aes(y = Rt_up), lwd=0.6) +
  geom_ribbon(data=last.date.old.wide.2, aes(ymin=Rt_low,ymax=Rt_up), fill="darkred",  alpha = 0.6) +
  geom_line(data=last.date.old.wide.2, aes(x=Date, y=Rt_avg, colour = "09 februari"), lwd=2) +    #nieuwe file --> 17
  
  
  ## old prediction
  geom_line(aes(y = Rt_low), lwd=0.6) +
  geom_line(aes(y = Rt_up), lwd=0.6) +
  geom_ribbon(aes(ymin=Rt_low, ymax=Rt_up), color="lightblue", fill="lightblue", alpha = 0.4) +
  geom_line(aes(y = Rt_avg, colour = "02 februari"), lwd=1.2)+
  
  
  scale_color_manual(values = c("darkblue", "darkred"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
  
  scale_x_date(date_breaks = "2 week",
               date_labels= format("%d-%b"),
               limits = as.Date(c("2020-08-01", today-5)))+
  
  labs(title = "Reproductiegetal, RIVM model runs",
       colour = "model run:",
       caption = paste("Bron: RIVM | Plot: @YorickB ",Sys.Date()))+
  
  
  theme_classic() + 
  xlab("")+ 
  ylab("")+
  
  theme(legend.position="top",
        legend.direction = "vertical",
        legend.title = element_text(color = "Black", face = "bold", size = 14),
        legend.text = element_text(color = "black",size = 14),
        legend.background = element_rect(fill = "#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.key = element_rect(fill = "#F5F5F5", color = NA)
        )+
  
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
         panel.grid.major.x = element_line(colour= "lightgray", linetype = "dashed")
         )+
  
  geom_hline(yintercept=1)

ggsave("data/41_reproduction_combi.png",width=16, height = 9)

File_date_7 <- paste0("data/",last.date.old.2,"_COVID-19_reproductiegetal_old.png")
ggsave(File_date_7,width=16, height = 9)


















#### other exernal factors




persco.df=data.frame(date=as.Date(c("2020-09-19", "2020-09-29", "2020-10-14", "2020-11-04","2020-11-18")), 
                     event=c("|Kroeg uurtje eerder dicht", "|We gaan voor R=0,9","|Semi-lockdown\n(0.75-0.99)", "|verzwaring\nsemi-lockdown\n(0.72-0.91)", "|Einde verzwaring"),
                     yas=c(1.01, 0.9, 0.75, 0.72,0.75)
)

#r.estimate.df = data.frame(date_start=as.Date(c("2020-09-19", "2020-09-29", "2020-10-14", "2020-11-04")), 
#                           date_end=as.Date(c("2020-11-17", "2020-10-14", "2020-11-17", "2020-11-17"))
#                         ) 


ggplot(last.date.old.wide.2, aes(x=Date, y=Rt_avg, group = 1))+
  
  geom_vline(data=persco.df, mapping=aes(xintercept=date), color="black", linetype = "dotted") +
  
  
  geom_line(aes(y = Rt_low), lwd=0.6) +
  geom_line(aes(y = Rt_up), lwd=0.6) +
  geom_ribbon(aes(ymin=Rt_low,ymax=Rt_up), fill="darkred", alpha = 0.2) +
  geom_line(aes(y = Rt_avg, color = "Effectieve R"), color = "black",lwd=2) +
  
  theme_classic() + 
  xlab("")+ 
  ylab("")+
  
  scale_x_date(limits=as.Date(c("2020-9-1", today)), date_breaks = "1 week", date_labels = "%b %d") +
  scale_y_continuous(expand = c(0, 0), limits = c(0.5, 1.5)) +
  
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()
  ) +
  
  labs(x = "Datum",
       y = "Reproductiegetal",
       color = "Legend") +
  
  labs(title = "Reproductiegetal, model 09 februari",
       subtitle = "Beleidsdoelen voor de R", #  OMT: 'Een lagere R is beter'",
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
  
  #annotate("rect", xmin = as.Date("2020-09-29"), xmax = as.Date("2020-10-14"), ymin = 0.95, ymax = 0.85, fill = "red", alpha = 0.3)+
  #annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2020-11-04"), ymin = 0.75, ymax = 0.99, fill = "blue", alpha = 0.2)+
  #annotate("rect", xmin = as.Date("2020-11-04"), xmax = as.Date("2020-11-18"), ymin = 0.72, ymax = 0.91, fill = "green", alpha = 0.3)+
  #annotate("rect", xmin = as.Date("2020-11-18"), xmax = as.Date("2020-12-03"), ymin = 0.75, ymax = 0.99, fill = "blue", alpha = 0.2)+
  
  annotate("rect", xmin = as.Date("2020-10-10"), xmax = as.Date("2020-10-18"), ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.2)+
  annotate("rect", xmin = as.Date("2020-10-17"), xmax = as.Date("2020-10-25"), ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.2)+
  
  geom_text(data=persco.df, mapping=aes(x=date, y=yas, label=event), size=6, angle=0, vjust=-0.4, hjust=0.012, face="bold")


ggsave("data/40_reproduction-vac-2.png",width=16, height = 9)












