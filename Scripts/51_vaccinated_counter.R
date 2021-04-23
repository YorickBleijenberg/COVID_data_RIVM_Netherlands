

vacc_quarter.file <- paste0("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/vaccination/people.vaccinated%20-%20leveringen.per.kwartaal.csv")
vacc_quarter <-read.csv(vacc_quarter.file,sep=",")
vacc_quarter$date <- as.Date(vacc_quarter$date)

vacc_quarter <- vacc_quarter[ -c(10:462),]
vacc_quarter <- vacc_quarter[ -c(9:20)]

vacc_quarter <- vacc_quarter[ -c(7:9),]

colnames(vacc_quarter) <- c("date","Pfizer/BioNTech", "Moderna","AstraZeneca","CureVac","Janssen","Sanofi/GSK", "Pfizer/BioNTech extra")


key <- "date"
value <- "vacc_type"
gathercols <- c("Pfizer/BioNTech", "Moderna","AstraZeneca","CureVac","Janssen","Sanofi/GSK", "Pfizer/BioNTech extra")
#gathercols <- c("Pfizer/BioNTech","AstraZeneca", "Moderna", "Pfizer/BioNTech extra")
vacc_quarter.long <- gather(vacc_quarter, key, value, gathercols)

vacc_quarter.long$key <- as.factor("Pfizer/BioNTech", "Moderna","AstraZeneca","CureVac","Janssen","Sanofi/GSK", "Pfizer/BioNTech extra")
# vacc_quarter.long$key <- as.factor("Pfizer/BioNTech","AstraZeneca", "Moderna", "Pfizer/BioNTech extra")

#### plot ###


ggplot(vacc_quarter.long,  aes(x=date, y=value, fill= factor(key, levels=c("Sanofi/GSK","CureVac","AstraZeneca","Janssen","Moderna", "Pfizer/BioNTech extra","Pfizer/BioNTech"))))+
  
  
  geom_vline(xintercept = as.Date("2021-04-01"), linetype = "dotted") + 
  geom_vline(xintercept = as.Date("2021-07-01"), linetype = "dotted") + 
  #geom_vline(xintercept = as.Date("2021-10-01"), linetype = "dotted") + 
  #geom_vline(xintercept = as.Date("2022-01-01"), linetype = "dotted") + 
  
  annotate("text", x = as.Date("2021-04-01"), y = 80000000, label = " Q2",      size=5, angle=0, vjust=-0.4,hjust = 0, color = "black")+
  annotate("text", x = as.Date("2021-07-01"), y = 80000000, label = " Q3",      size=5, angle=0, vjust=-0.4,hjust = 0, color = "black")+
 # annotate("text", x = as.Date("2021-10-01"), y = 80000000, label = " Q4",      size=5, angle=0, vjust=-0.4,hjust = 0, color = "black")+
#  annotate("text", x = as.Date("2022-01-01"), y = 80000000, label = " 2022",    size=5, angle=0, vjust=-0.4,hjust = 0, color = "black")+
  
  
  
  geom_area(position = "stack", alpha = 0.8)+
  geom_line(position = "stack", size=1)+

  
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
#  scale_y_continuous(limits = c(0, 86000000),breaks = c(5000000, 10000000, 20000000,18000000,40000000, 50000000, 80000000), labels = label_comma(big.mark = ".", decimal.mark = ","))+
  scale_y_continuous(limits = c(0, 25000000),breaks = c(5000000, 10000000, 20000000,18000000,40000000, 50000000, 15000000), labels = label_comma(big.mark = ".", decimal.mark = ","))+
  

 # scale_x_date(date_breaks = "1 month", 
   #            date_labels= format("%b"),
           #    limits = as.Date(c("2020-12-15", "2022-06-31")))+
  #
  coord_cartesian(expand = FALSE)+
  
  labs(title = "Verwachting aantal geleverde doses",
       subtitle = "op basis van de Kamerbrief, 13-4-2021 & NOS",
       caption = paste("Source: min VWS | Plot: @YorickB | ",Sys.Date()))+
  
  theme(legend.position =  "top",
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.margin = margin(3, 3, 3, 3),
        legend.text = element_text(colour="black", size=20, face="bold"))+
  
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         plot.title = element_text(hjust = 0.5,size = 32,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=12),  #, angle=45),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
         axis.line = element_line(colour = "#F5F5F5"),
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
ggsave("data/90_vaccine_deliverd-june.png",width=16, height = 9)  







##### Full ######



vacc_quarter.file <- paste0("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/vaccination/people.vaccinated%20-%20leveringen.per.kwartaal.csv")
vacc_quarter <-read.csv(vacc_quarter.file,sep=",")


vacc_quarter <- vacc_quarter[ -c(10:462),]
vacc_quarter <- vacc_quarter[ -c(1:12)]

vacc_quarter <- vacc_quarter[ -c(7:9),]


colnames(vacc_quarter) <- c("date","Pfizer/BioNTech", "Moderna","AstraZeneca","CureVac","Janssen","Sanofi/GSK", "Pfizer/BioNTech extra")
vacc_quarter$date <- as.Date(vacc_quarter$date)

key <- "date"
value <- "vacc_type"
gathercols <- c("Pfizer/BioNTech", "Moderna","AstraZeneca","CureVac","Janssen","Sanofi/GSK", "Pfizer/BioNTech extra")
vacc_quarter.long <- gather(vacc_quarter, key, value, gathercols)

vacc_quarter.long$key <- as.factor("Pfizer/BioNTech", "Moderna","AstraZeneca","CureVac","Janssen","Sanofi/GSK", "Pfizer/BioNTech extra")

#### plot ###


ggplot(vacc_quarter.long,  aes(x=date, y=value, fill= factor(key, levels=c("Sanofi/GSK","CureVac","AstraZeneca","Janssen","Moderna", "Pfizer/BioNTech extra","Pfizer/BioNTech"))))+
  
  
  geom_vline(xintercept = as.Date("2021-04-01"), linetype = "dotted") + 
  geom_vline(xintercept = as.Date("2021-07-01"), linetype = "dotted") + 
  #geom_vline(xintercept = as.Date("2021-10-01"), linetype = "dotted") + 
 # geom_vline(xintercept = as.Date("2022-01-01"), linetype = "dotted") + 
  
  annotate("text", x = as.Date("2021-04-01"), y = 42000000, label = " Q2",      size=5, angle=0, vjust=-0.4,hjust = 0, color = "black")+
  annotate("text", x = as.Date("2021-07-01"), y = 42000000, label = " Q3",      size=5, angle=0, vjust=-0.4,hjust = 0, color = "black")+
 # annotate("text", x = as.Date("2021-10-01"), y = 42000000, label = " Q4",      size=5, angle=0, vjust=-0.4,hjust = 0, color = "black")+
  #annotate("text", x = as.Date("2022-01-01"), y = 45000000, label = " 2022",    size=5, angle=0, vjust=-0.4,hjust = 0, color = "black")+
  
  geom_area(position = "stack", alpha = 0.8)+
  geom_line(position = "stack", size=1)+
  
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
 # scale_y_continuous(limits = c(0, 50000000),breaks = c(12250000, 2500000, 5000000, 10000000, 20000000,30000000,40000000), labels = label_comma(big.mark = ".", decimal.mark = ","))+
  scale_y_continuous(limits = c(0, 14000000),breaks = c(12250000, 2500000, 5000000, 10000000, 20000000,30000000,7500000), labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  
    scale_x_date(date_breaks = "1 month", 
               date_labels= format("%b"),
               limits = as.Date(c("2020-12-15", "2021-06-31")))+
  
  coord_cartesian(expand = FALSE)+
  
  labs(title = "Verwachte aantallen mensen die we volledig* kunnen vaccineren",
       subtitle = "op basis van de Kamerbrief, 13-4-2021 & NOS \n *voor Janssen 1 dosis nodig; voor de andere vaccins 2 doses",
       caption = paste("Source: min VWS | Plot: @YorickB | ",Sys.Date()))+
  
  theme(legend.position =  "top",
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.margin = margin(3, 3, 3, 3),
        legend.text = element_text(colour="black", size=20, face="bold"))+
  
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         plot.title = element_text(hjust = 0.5,size = 32,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=12),  #, angle=45),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
         axis.line = element_line(colour = "#F5F5F5"),
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
ggsave("data/90_vaccine_deliverd-full-june.png",width=16, height = 9)  

