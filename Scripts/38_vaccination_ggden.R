


today <- Sys.Date()
ggd_gh_file <- paste0("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/vaccination/ggd/",today,"%20-%20people.vaccinated%20-%20GGD'en.csv")

people.vaccinated.ggd.gh <-read.csv(ggd_gh_file,sep=",")
people.vaccinated.ggd.gh$date <- as.Date(people.vaccinated.ggd.gh$date)
#####
yesterday_new <- last(people.vaccinated.ggd.gh$new)
yesterday_new <- format(yesterday_new, big.mark="." ,decimal.mark=",")
subtitle_text_new <- paste("Gisteren gevaccineerd:", yesterday_new,"\nWe moeten naar 100.000+ per dag (1 miljoen per week)")
#####

ggplot(people.vaccinated.ggd.gh)+
  geom_col(aes(x=date, y=new), fill = "darkgreen")+
  scale_x_date(date_breaks = "1 weeks", 
               date_labels= format("%d/%m"),
               limits = as.Date(c("2021-01-19", "2021-02-08")))+
  scale_y_continuous(limits = c(0, NA), labels = label_comma(big.mark = ".", decimal.mark = ","))+  #breaks = c(2500, 5000, 7500,10000,12500,15000),
  coord_cartesian(expand = FALSE)+
  theme_classic()+
  xlab("")+
  ylab("")+
  
  labs(title = "GGD'en: Vaccinaties per dag",
       subtitle = subtitle_text_new,
       caption = paste("Bron: github.com/YorickBleijenberg | Plot: @YorickB  | ",Sys.Date()))+
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    legend.position = "none",   # no legend
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
  
  ggsave("data/93_vaccinated_ggd_new.png",width=16, height = 9)  


####  total GGD'en Plot ####

yesterday_total_first <- last(people.vaccinated.ggd.gh$total_1st )
yesterday_total_first <- format(yesterday_total_first, big.mark="." ,decimal.mark=",")
subtitle_text_total_first <- paste("Totaal GGD'en:", yesterday_total_first)

upperlimit <- last(people.vaccinated.ggd.gh$total_1st)+10000

#####
ggplot(people.vaccinated.ggd.gh)+

  geom_line(aes(x=date, y=total_1st), color = "darkgreen", size = 3)+
  geom_point(aes(x=date, y=total_1st), color = "darkgreen", size = 5)+
  geom_point(aes(x=date, y=total_1st), color = "black", size = 3)+
  
   scale_x_date(date_breaks = "1 day", 
                date_labels= format("%d/%m"),
                limits = as.Date(c("2021-01-19", "2021-02-7")))+
  scale_y_continuous(limits = c(0, upperlimit), labels = label_comma(big.mark = ".", decimal.mark = ","))+  #breaks = c(2500, 5000, 7500,10000,12500,15000),
  coord_cartesian(expand = FALSE)+
  theme_classic()+
  xlab("")+
  ylab("")+
  
  labs(title = "GGD'en: totaal aantal prikken",
       subtitle = subtitle_text_total_first,
       caption = paste("Bron: github.com/YorickBleijenberg | Plot: @YorickB  | ",Sys.Date()))+
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    legend.position = "none",   # no legend
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
  
  ggsave("data/93_vaccinated_ggd_total.png",width=16, height = 9)  

