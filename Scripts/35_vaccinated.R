library(scales)

today <- Sys.Date()


people.vaccinated.gh <-read.csv("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/vaccination/people.vaccinated.csv",sep=",")
people.vaccinated.gh$date <- as.Date(people.vaccinated.gh$date)
vaccinated.people <- last(people.vaccinated.gh$total_vaccinations)
vac.perc <-  round((vaccinated.people/17474677*100), digits =5)
vac.perc <- format(vac.perc, scientific=F)

total.vaccinated <- read.csv("C:\\Rdir\\data-contstant\\vaccination.planning.csv" ,sep=";")

colnames(total.vaccinated) = c("Datum", "infected", "seroprevlence",
                               "Pfizer/BioNTech", "Pfizer_deliver",
                               "Moderna", "Moderna_deliver",
                               "AstraZeneca", "AstraZenica_deliver",
                               "CureVac","CureVac_deliver", 
                               "Janssen_2dose","Janssen",
                               "Sanofi/GSK", "Sanofi/GSK_deliver",
                               "Moderna top-up", "Moderna top-up_deliver",
                               "Novavax", "Novavax_deliver",
                               "Valneva", "Valneva_deliver",
                               "total.avail","total.pop",
                               "60p", "70p", "80p","90p"
                               )
total.vaccinated$Datum <- as.Date(total.vaccinated$Datum)


test.phizer <- filter(total.vaccinated[(total.vaccinated$Datum == today),])
test.phizer <- test.phizer[1,]

test.total <- test.phizer$Pfizer_deliver + test.phizer$Moderna_deliver

num.vaccinated = vaccinated.people
num.vaccinated.full = 0

num.vaccinated.perc <-  vac.perc
num.vaccinated.full.perc <-  0      ##as.numeric(num.vaccinated/17461543*100)

test.total     <- format( test.total, big.mark="." ,decimal.mark=",")
num.vaccinated <- format( num.vaccinated, big.mark="." ,decimal.mark=",")
num.vaccinated.full <- format( num.vaccinated.full, big.mark="." ,decimal.mark=",")
num.vaccinated.perc <- format( num.vaccinated.perc, big.mark="." ,decimal.mark=",")

num.vaccinated.label      <- paste0("aantal mensen gevaccineerd: ", num.vaccinated, " = ", num.vaccinated.perc,"%")
num.vaccinated.full.label <- paste0("aantal mensen volledig gevaccineerd: ", num.vaccinated.full, " = ", num.vaccinated.full.perc,"%")
num.vaccine.avail.label <- paste("aantalen doses geleverd (schatting volgens planning):",test.total)




total.vaccinated[is.na(total.vaccinated)] <- 0
total.vaccinated$Janssen <- as.integer(total.vaccinated$Janssen)

#total.vaccinated.short.def <-  total.vaccinated[ -c(2,3,5,7,9,11,12,15,17:23)]
#total.vaccinated.short.def$Janssen <- as.integer(total.vaccinated.short$Janssen)


keycol <- "Datum"
valuecol <- "type"
gathercols <- c("Pfizer/BioNTech","AstraZeneca", "Moderna","CureVac","Janssen","Sanofi/GSK")


total.vaccinated.gather <- gather(total.vaccinated, keycol, valuecol, gathercols)
total.vaccinated.gather$Datum <- as.Date(total.vaccinated.gather$Datum)
total.vaccinated.gather$valuecol <- as.integer(total.vaccinated.gather$valuecol)



##### plot ####

ggplot(total.vaccinated.gather,aes( x=Datum, y=valuecol, fill = keycol))+

  geom_hline(yintercept=17474677, color = "green", size = 2)+
  geom_hline(yintercept=15727209, color = "yellow", size = 2)+
  geom_hline(yintercept=13979741, color = "orange", size = 2)+
  geom_hline(yintercept=12232273, color = "darkred", size = 2)+
  geom_hline(yintercept=10484806, color = "black", size = 2)+
  
  annotate("text", x = as.Date("2021-01-01"), y = 17761543, label = "100%", size=5,color = "black")+
  annotate("text", x = as.Date("2021-01-01"), y = 16015388, label = "90%", size=5,color = "black")+
  annotate("text", x = as.Date("2021-01-01"), y = 14269234, label = "80%", size=5,color = "black")+
  annotate("text", x = as.Date("2021-01-01"), y = 12823080, label = "70%", size=5,color = "black")+
  annotate("text", x = as.Date("2021-01-01"), y = 11006925, label = "60%", size=5,color = "black")+
  
  geom_bar(position="stack", stat="identity", width=1, alpha=0.8)+  # geom_bar(stat = "identity", aes( x=Datum, y=valuecol, fill = keycol ))+
  geom_line(position="stack", stat="identity")+
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
  scale_y_continuous(limits = c(0, 37000000),breaks = c(2500000, 5000000, 7500000,10000000, 12500000,15000000, 17461543, 20000000,25000000,30000000,35000000), labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  geom_vline(xintercept = as.Date(today), linetype = "dotted") + 
  #geom_text(mapping=aes(x=as.Date(today), y=num.vaccinated, label=num.vaccine.avail.label), size=3, angle=-90, vjust=-0.4, hjust=1.1)+
  #geom_text(mapping=aes(x=as.Date(today), y=num.vaccinated, label=num.vaccinated.label), size=3, angle=-90, vjust=1, hjust=1.1)+
  
  annotate("text", x = as.Date(today+1), y = 28000000, label = today,                     size=4, angle=0, vjust=-0.4, hjust = 0, color = "black")+
  annotate("text", x = as.Date(today+1), y = 27000000, label = num.vaccinated.label,      size=3, angle=0, vjust=-0.4,hjust = 0, color = "black")+
 # annotate("text", x = as.Date(today+1), y = 26950000, label = num.vaccinated.full.label, size=3, angle=0, vjust=1, hjust = 0,color = "black")+
  annotate("text", x = as.Date(today+1), y = 26025000, label = num.vaccine.avail.label,   size=3, angle=0, vjust=1, hjust = 0,color = "black")+
  
  geom_vline(xintercept = as.Date("2021-07-22"), linetype = "dotted") + 
  annotate("text", x = as.Date("2021-07-23"), y = 28000000, label = "2021-07-21",                     size=4, angle=0, vjust=-0.4, hjust = 0, color = "black")+
  annotate("text", x = as.Date("2021-07-23"), y = 27000000, label = "100%  (in theorie)",      size=3, angle=0, vjust=-0.4,hjust = 0, color = "black")+
  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%b"),
               limits = as.Date(c("2020-12-15", "2021-12-31")))+
  
  coord_cartesian(expand = FALSE)+
  
  labs(title = "Verwachte aantallen mensen die we volledig* kunnen vaccineren",
       subtitle = "op basis van de Kamerbrief, 3-2-2021 \n *voor Janssen 1 dosis nodig; voor de andere vaccins 2 doses",
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

  ggsave("data/90_vaccine_deliverd.png",width=16, height = 9)

  






keycol <- "Datum"
valuecol <- "type"
gathercols <- c("Pfizer/BioNTech","AstraZeneca", "Moderna","CureVac","Janssen","Sanofi/GSK","Moderna top-up", "Novavax","Valneva")


total.vaccinated.gather.pot <- gather(total.vaccinated, keycol, valuecol, gathercols)
total.vaccinated.gather.pot$Datum <- as.Date(total.vaccinated.gather.pot$Datum)
total.vaccinated.gather.pot$valuecol <- as.integer(total.vaccinated.gather.pot$valuecol)




##### plot ####

ggplot(total.vaccinated.gather.pot,aes( x=Datum, y=valuecol, fill = keycol))+
  
  geom_hline(yintercept=17474677, color = "green", size = 2)+
  geom_hline(yintercept=15727209, color = "yellow", size = 2)+
  geom_hline(yintercept=13979741, color = "orange", size = 2)+
  geom_hline(yintercept=12232273, color = "darkred", size = 2)+
  geom_hline(yintercept=10484806, color = "black", size = 2)+
  
  annotate("text", x = as.Date("2021-01-01"), y = 17761543, label = "100%", size=5,color = "black")+
  annotate("text", x = as.Date("2021-01-01"), y = 16015388, label = "90%", size=5,color = "black")+
  annotate("text", x = as.Date("2021-01-01"), y = 14269234, label = "80%", size=5,color = "black")+
  annotate("text", x = as.Date("2021-01-01"), y = 12823080, label = "70%", size=5,color = "black")+
  annotate("text", x = as.Date("2021-01-01"), y = 11006925, label = "60%", size=5,color = "black")+
  
  geom_bar(position="stack", stat="identity", width=1, alpha=0.8)+  # geom_bar(stat = "identity", aes( x=Datum, y=valuecol, fill = keycol ))+
  geom_line(position="stack", stat="identity")+
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
  scale_y_continuous(limits = c(0, 50000000),breaks = c(2500000, 5000000, 7500000,10000000, 12500000,15000000, 17461543, 20000000,25000000,30000000,35000000,40000000,45000000), labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  geom_vline(xintercept = as.Date(today), linetype = "dotted") + 
  #geom_text(mapping=aes(x=as.Date(today), y=num.vaccinated, label=num.vaccine.avail.label), size=3, angle=-90, vjust=-0.4, hjust=1.1)+
  #geom_text(mapping=aes(x=as.Date(today), y=num.vaccinated, label=num.vaccinated.label), size=3, angle=-90, vjust=1, hjust=1.1)+
  
  annotate("text", x = as.Date(today+1), y = 28000000, label = today,                     size=4, angle=0, vjust=-0.4, hjust = 0, color = "black")+
  annotate("text", x = as.Date(today+1), y = 27000000, label = num.vaccinated.label,      size=3, angle=0, vjust=-0.4,hjust = 0, color = "black")+
  # annotate("text", x = as.Date(today+1), y = 26950000, label = num.vaccinated.full.label, size=3, angle=0, vjust=1, hjust = 0,color = "black")+
  annotate("text", x = as.Date(today+1), y = 26025000, label = num.vaccine.avail.label,   size=3, angle=0, vjust=1, hjust = 0,color = "black")+
  
  geom_vline(xintercept = as.Date("2021-07-17"), linetype = "dotted") + 
  annotate("text", x = as.Date("2021-07-18"), y = 28000000, label = "2021-07-21",                     size=4, angle=0, vjust=-0.4, hjust = 0, color = "black")+
  annotate("text", x = as.Date("2021-07-18"), y = 27000000, label = "100%  (in theorie)",      size=3, angle=0, vjust=-0.4,hjust = 0, color = "black")+
  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%b"),
               limits = as.Date(c("2020-12-15", "2022-01-10")))+
  
  coord_cartesian(expand = FALSE)+
  
  labs(title = "Verwachte aantallen mensen die we volledig* kunnen vaccineren",
       subtitle = "op basis van de Kamerbrief, 3-2-2021 \n *voor Janssen 1 dosis nodig; voor de andere vaccins 2 doses",
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
  
  ggsave("data/90_vaccine_deliverd_expect.png",width=16, height = 9)


agecbs.df <-read.csv("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/data-cbs/people.vaccinated%20-%20age-reverse.csv",sep=",")


##### plot ####

ggplot(total.vaccinated.gather,aes( x=Datum, y=valuecol, fill = keycol))+
  
 
  geom_hline(data=agecbs.df, mapping=aes(yintercept=aantal), color="black")+
  geom_text(data=agecbs.df, mapping=aes(x=as.Date("2021-01-01"), y=aantal, label=Leeftijd), size=4, vjust=-0.4, hjust=0)+
  
  
  geom_bar(position="stack", stat="identity", width=1, alpha=0.8)+  # geom_bar(stat = "identity", aes( x=Datum, y=valuecol, fill = keycol ))+
  geom_line(position="stack", stat="identity")+
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
  scale_y_continuous(limits = c(0, 37000000),breaks = c(2500000, 5000000, 7500000,10000000, 12500000,15000000, 17461543, 20000000,25000000,30000000,35000000), labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  geom_vline(xintercept = as.Date(today), linetype = "dotted") + 
  #geom_text(mapping=aes(x=as.Date(today), y=num.vaccinated, label=num.vaccine.avail.label), size=3, angle=-90, vjust=-0.4, hjust=1.1)+
  #geom_text(mapping=aes(x=as.Date(today), y=num.vaccinated, label=num.vaccinated.label), size=3, angle=-90, vjust=1, hjust=1.1)+
  
  annotate("text", x = as.Date(today+1), y = 28000000, label = today,                     size=4, angle=0, vjust=-0.4, hjust = 0, color = "black")+
  annotate("text", x = as.Date(today+1), y = 27000000, label = num.vaccinated.label,      size=3, angle=0, vjust=-0.4,hjust = 0, color = "black")+
  # annotate("text", x = as.Date(today+1), y = 26950000, label = num.vaccinated.full.label, size=3, angle=0, vjust=1, hjust = 0,color = "black")+
  annotate("text", x = as.Date(today+1), y = 26025000, label = num.vaccine.avail.label,   size=3, angle=0, vjust=1, hjust = 0,color = "black")+
  
  geom_vline(xintercept = as.Date("2021-07-22"), linetype = "dotted") + 
  annotate("text", x = as.Date("2021-07-23"), y = 28000000, label = "2021-07-21",                     size=4, angle=0, vjust=-0.4, hjust = 0, color = "black")+
  annotate("text", x = as.Date("2021-07-23"), y = 27000000, label = "100%  (in theorie)",      size=3, angle=0, vjust=-0.4,hjust = 0, color = "black")+
  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%b"),
               limits = as.Date(c("2020-12-15", "2021-12-31")))+
  
  coord_cartesian(expand = FALSE)+
  
  labs(title = "Verwachte aantallen mensen die we volledig* kunnen vaccineren",
       subtitle = "op basis van de Kamerbrief, 3-2-2021 \n *voor Janssen 1 dosis nodig; voor de andere vaccins 2 doses",
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
  
  ggsave("data/90_vaccine_deliverd_age.png",width=16, height = 9)

