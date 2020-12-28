library(scales)


total.vaccinated <- read.csv("C:\\Rdir\\data-contstant\\total.vaccinated.csv" ,sep=";")
colnames(total.vaccinated) = c("Datum", "infected", "seroprevlence",
                               "Pfizer/BioNTech", "Pfizer_deliver",
                               "Moderna", "Moderna_deliver",
                               "AstraZenica", "AstraZenica_deliver",
                               "CureVac","CureVac_deliver", 
                               "Janssen","Janssen_deliver",
                               "total.vaccinated","total.pop",
                               "60p", "70p", "80p","90p"
                               )
total.vaccinated$Datum <- as.Date(total.vaccinated$Datum)

total.vaccinated[is.na(total.vaccinated)] <- 0

total.vaccinated.short <-  total.vaccinated[ -c(2,3,5,7,9,11,13:19)]
total.vaccinated.short$Janssen <- as.integer(total.vaccinated.short$Janssen)


keycol <- "Datum"
valuecol <- "type"
gathercols <- c("Pfizer/BioNTech","AstraZenica", "Moderna","CureVac","Janssen")

total.vaccinated.gather <- gather(total.vaccinated.short, keycol, valuecol, gathercols)
total.vaccinated.gather$Datum <- as.Date(total.vaccinated.gather$Datum)

##### plot ####

ggplot(total.vaccinated.gather,aes( x=Datum, y=valuecol, fill = keycol))+

  geom_hline(yintercept=17461543, color = "green", size = 2)+
  geom_hline(yintercept=15715388, color = "yellow", size = 2)+
  geom_hline(yintercept=13969234, color = "orange", size = 2)+
  geom_hline(yintercept=12223080, color = "darkred", size = 2)+
  geom_hline(yintercept=10476925, color = "black", size = 2)+
  
  annotate("text", x = as.Date("2021-01-01"), y = 17761543, label = "100%", size=5,color = "black",face = "bold")+
  annotate("text", x = as.Date("2021-01-01"), y = 16015388, label = "90%", size=5,color = "black",face = "bold")+
  annotate("text", x = as.Date("2021-01-01"), y = 14269234, label = "80%", size=5,color = "black",face = "bold")+
  annotate("text", x = as.Date("2021-01-01"), y = 12523080, label = "70%", size=5,color = "black",face = "bold")+
  annotate("text", x = as.Date("2021-01-01"), y = 10776925, label = "60%", size=5,color = "black",face = "bold")+
  
  geom_bar(position="stack", stat="identity", width=1)+  # geom_bar(stat = "identity", aes( x=Datum, y=valuecol, fill = keycol ))+
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
  scale_y_continuous(limits = c(0, 18000000),breaks = c(2500000, 5000000, 7500000,10000000, 12500000,15000000, 17461543), labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%b"),
               limits = as.Date(c("2020-12-27", "2022-01-15")))+
  
  labs(title = "Aantal mensen gevaccineerd:",
       #subtitle = paste("7-daags zwevend gemiddele | Actueel tot:", Last_date_in_Google_file, "\n - semi-lockdown op 14 oktober\n - einde herfstvakantie op 25 oktober \n"),
       caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme(legend.position =  "top",
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.margin = margin(3, 3, 3, 3),
        legend.text = element_text(colour="black", size=20, face="bold"))+
  
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=12),  #, angle=45),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
         axis.line = element_line(colour = "#F5F5F5"),
       
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))
 
  
  ggsave("data/90_total_vaccinated.png",width=16, height = 9)

#  ggsave("data/90_total_vaccinated.png",width=15, height = 9)





