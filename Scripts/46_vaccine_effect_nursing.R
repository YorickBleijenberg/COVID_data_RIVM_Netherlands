
n= 113

new.date <- Sys.Date()-n
old.date <- new.date-1

###  2020-11-08

old.casus.file <-paste("https://raw.githubusercontent.com/mzelst/covid-19/master/data-rivm/nursing-homes-per-day/nursery_daily_", old.date, ".csv", sep = "")
old.casus <- read.csv(old.casus.file,sep=",")


n= 113


while (n > -1){

new.date <- Sys.Date()-n

new.casus.file <-paste("https://raw.githubusercontent.com/mzelst/covid-19/master/data-rivm/nursing-homes-per-day/nursery_daily_", new.date, ".csv", sep = "")
new.casus <- read.csv(new.casus.file,sep=",")

old.casus <- rbind(old.casus, new.casus)


n <- n-1

}
 


####cassus.age.spread.base  <- read.csv("C:\\Rdir\\data-contstant\\casus_daily_age_dif.csv" ,sep=";", check.names = FALSE)


File_date_5 <- paste0("data/casus_vac_effect/nursing_daily_diff-csv.csv")
write.csv2(old.casus, File_date_5, row.names=FALSE)



to.play.care.death <- old.casus
to.play.care.death$date  <- as.Date(to.play.care.death$date)

to.play.care.death$MAdeaths_care <- rollmeanr(to.play.care.death$deaths_today, 7, fill = 0)
to.play.care.death$MAdeaths_care_lead  <- lead(to.play.care.death$MAdeaths_care,3)

maxDeathCare   <- 60.28571    # max(to.play.care.death$deaths_today, na.rm = TRUE)
to.play.care.death$deaths_today.relative  <- casus.age.dif.play$deaths_today / maxDeathCare




ggplot(to.play.care.death, aes(x=date))+
  
  #geom_ribbon(data= relative.table.short, aes(x= dateInTable, ymin=cases_vplg_rel, ymax=cases_rel), fill="green", alpha = 0.5) +
 # geom_line(aes(y = cases_rel), lwd=4, color="#F5F5F5") +
 # geom_line(aes(y = cases_rel), lwd=3, color="#1F968BFF") +
  
  geom_line(data = Merged_data_short, mapping = aes(x=fixedDate, y=ma_d_lead), color = "#F5F5F5",lwd = 3)+
  geom_line(data = Merged_data_short,mapping = aes(x=fixedDate, y=ma_d_lead), color = "#ff0505",lwd = 2)+
  
  
  
  geom_point(aes(y = deaths_today), lwd=4, color="#F5F5F5") +
  geom_point(aes(y = deaths_today), lwd=3, color="black") +
  
  geom_line(aes(y = MAdeaths_care_lead), lwd=4, color="#F5F5F5") +
  geom_line(aes(y = MAdeaths_care_lead), lwd=3, color="#481567FF") +
  
 # annotate("text", x = as.Date("2021-02-22"), y = 0.43, label = "Totaal",         size=5, face = "bold", color = "#1F968BFF")+
 # annotate("text", x = as.Date("2021-02-13"), y = 0.18, label = "Verpleeghuizen", size=5, face = "bold", color = "#481567FF")+
  
  scale_y_continuous(limits = as.Date(c(0, 100)))+  #labels = percent, 
  scale_x_date(date_breaks = "1 months",date_labels= format("%b"),
               limits = as.Date(c("2020-11-15", NA)))+
  
  #coord_cartesian(expand = FALSE)+
  
  theme_classic()+
  
  xlab("")+
  ylab("")+
  
  geom_vline(xintercept = as.Date("2021-01-18"), linetype = "dotted") + 
  annotate("text", x = as.Date("2021-01-19"), y = 0.8, label = "start vaccinatie verpleeghuizen", size=4, angle=0, vjust=-0.4, hjust = 0, color = "black")+
  
  #scale_color_manual( values=c("#1F968BFF", "#481567FF"), labels=c("totaal", "verpleeghuizen" ))+
  
  labs(title = "Verpleeghuizen & Totaal",
       subtitle = subtitle.care.label,
       caption = paste("Bron: RIVM | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(#legend.position = "none",   # no legend
    legend.title = element_blank(),  ## legend title
    legend.position="top",
    legend.direction = "vertical",
    legend.background = element_rect(fill="#f5f5f5", size=0.5, linetype="solid"))+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title =     element_text(hjust = 0.5 ,size = 40 ,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5   ,size = 15 ,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
  ggsave("data/73_leeftijd_relatief_care_death.png",width=16, height = 9)

