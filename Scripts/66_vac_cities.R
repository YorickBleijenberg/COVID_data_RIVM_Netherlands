FR_a <- intToUtf8(0x00E2)  #Encoding(FR_b) <- "UTF-8"
FR_u <- intToUtf8(0x00FA)  #Encoding(FR_b) <- "UTF-8"
SFR_name <- paste0("S", FR_u, "dwest Frysl", FR_a,"n")
SFR_name2 <- paste0("Noardeast-Frysl", FR_a,"n")
SFR_name3 <- paste0("Frysl", FR_a,"n")


vaccdata_json <- "https://data.rivm.nl/data/covid-19/COVID-19_vaccinatiegraad_per_gemeente_per_week_leeftijd.json"
vaccdata_dat <- fromJSON(txt = vaccdata_json)
vaccdata_dat <- vaccdata_dat [, -c(1:4,7)]  
vaccdata_dat.18p <- (vaccdata_dat %>% filter(Age_group == "18+" ))
colnames(vaccdata_dat.18p) <- c("Municipality_code", "Gemeente", "Eerste dosis", "volledige.vaccinatie", "leeftijd")
vaccdata_dat.18p.full <- vaccdata_dat.18p [, -c(2,3,5)]  

vaccdata_dat.18p.full$volledige.vaccinatie <- as.integer(vaccdata_dat.18p.full$volledige.vaccinatie)


inwo_gem <- "C:\\Rdir\\data-contstant\\CBS_inwoners_gemeente.csv"
gemeente.inwoners <- read.csv(inwo_gem,sep=";")  
colnames(gemeente.inwoners) = c("Municipality_code", "Gemeente_Naam", "inwoners", "gemeente_getal")
gemeente.inwoners$Gemeente_Naam <- str_replace(gemeente.inwoners$Gemeente_Naam, "Súdwest Fryslân", SFR_name)  ##fout / goed
gemeente.inwoners$Gemeente_Naam <- str_replace(gemeente.inwoners$Gemeente_Naam, "Noardeast-Fryslân", SFR_name2)  ##fout / goed
gemeente.inwoners <- gemeente.inwoners [, -c(4)]


provincie.tab <- "C:\\Rdir\\data-contstant\\cbs_gemeenten-alfabetisch-2021.csv"
provincie.tabel <- read.csv(provincie.tab, sep=";")  
provincie.tabel <- provincie.tabel [, -c(1,3:5)]
colnames(provincie.tabel) <- c("Municipality_code", "provincie")
provincie.tabel$provincie <- str_replace(provincie.tabel$provincie, "Fryslân", SFR_name3)  ##fout / goed


seven.day.incidence.table <- RIVM_aantallen_gemeente_per_dag.combi.3
seven.day.incidence.table <-   (seven.day.incidence.table %>% filter(date == today )) 
seven.day.incidence.table <- seven.day.incidence.table [, -c(1,2,4,6)]
colnames(seven.day.incidence.table) = c("Gemeente_Naam", "incidentie")

Gemeente.size.combi <-   merge(gemeente.inwoners, provincie.tabel)
Gemeente.size.combi <- merge(Gemeente.size.combi, vaccdata_dat.18p.full)
Gemeente.size.combi <- merge(Gemeente.size.combi, seven.day.incidence.table, )








File_date_vac.cit <- paste0("data/",format(Sys.Date(), "%Y-%m-%d"), "_vac.cities.csv")
write.csv2(Gemeente.size.combi, File_date_vac.cit, row.names=FALSE)


Gemeente.size.combi$volledige.vaccinatie = Gemeente.size.combi$volledige.vaccinatie/100



#### plot #####


ggplot(Gemeente.size.combi, aes(x=inwoners, y = volledige.vaccinatie))+
  geom_point(size=2)+ 
  scale_x_continuous(trans='log2', breaks = c(1000,2500, 5000,10000,25000,50000,100000,250000,500000,750000))+
  scale_y_continuous(limits = c(0, 1), label = percent_format())+ 
  

  
  labs(title = "Inwoners vs. vaccinatiegraad",
       subtitle = paste("X-as logaritmisch"),
       caption = paste("bron: RIVM  | Plot: @YorickB | ",Sys.Date()))+
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title = element_text(hjust = 0.5,size = 40,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
ggsave("data/plots/vac.cities.png",width=16, height = 9)
  