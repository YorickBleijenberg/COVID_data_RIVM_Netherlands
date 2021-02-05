
people.vaccinated.gh = 1
people.vaccinated.gh <-read.csv("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/vaccination/people.vaccinated-new.csv",sep=",")
people.vaccinated.gh$date <- as.Date(people.vaccinated.gh$date)

vaccinated.people <- last(people.vaccinated.gh$total_vaccinations)

people.vaccinated.gh$MANew  <- rollmeanr(people.vaccinated.gh$new_vaccinations, 7, fill = 0)
people.vaccinated.gh$ma_new_lead  <- lead(people.vaccinated.gh$MANew,3)

MA.New <- as.integer( last(people.vaccinated.gh$MANew))
MA.New.phd <- round((MA.New/17474677*100), digits =4)
MA.New <- format(MA.New, big.mark="." ,decimal.mark=",")
MA.subtitle <- paste("7-daags gemiddelde:", MA.New,"- dat is:",MA.New.phd,"per 100 mensen","\nWe moeten naar 100.000+ per dag (1 miljoen per week)")


ggplot(people.vaccinated.gh)+
  geom_col(aes(x=date, y=new_vaccinations))+ #, fill = "darkgreen")+
  
  geom_line(aes(x=date, y=ma_new_lead), size =3, color = "#DAE3F3")+
  geom_line(aes(x=date, y=ma_new_lead), size =2)+
  
  scale_x_date(date_breaks = "1 weeks", 
               date_labels= format("%d/%m"),
               limits = as.Date(c("2021-01-01", "2021-01-30")))+
scale_y_continuous(limits = c(0, 150000), labels = label_comma(big.mark = ".", decimal.mark = ","))+  #breaks = c(2500, 5000, 7500,10000,12500,15000),
  coord_cartesian(expand = FALSE)+
  theme_classic()+
  xlab("")+
  ylab("")+
  
  labs(title = "Vaccinaties per dag",
       subtitle = MA.subtitle,
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

ggsave("data/93_vaccinated_new.png",width=16, height = 9)  



#vaccinated.people = 4
vac.perc <-  round((vaccinated.people/17474677*100), digits =4)
vac.perc <- format(vac.perc, scientific=F)

vaccinated.people  <- format( vaccinated.people, big.mark="." ,decimal.mark=",")
vaccinated.people.label <- paste0("Aantal mensen gevaccineerd: ",vaccinated.people)
vacccinated.percentage <- paste0("Percentage Nederlanders gevaccineerd: ",vac.perc,"%")

placeholder <- data.frame(x = 0:16, y = 0:16)

ggplot(placeholder)+
  geom_area(aes(x=x,y=y), fill = "#F5F5F5")+
  geom_text(mapping=aes(x=0.5, y=12, label=vaccinated.people.label), size=15, angle=0, vjust=0, hjust=0, color= "darkblue")+
  geom_text(mapping=aes(x=0.5, y=8, label="Aantal mensen volledig (2 doses) gevaccineerd: 0"), size=15, angle=0, vjust=0, hjust=0, color= "darkblue")+
  geom_text(mapping=aes(x=0.5, y=4, label=vacccinated.percentage), size=15, angle=0, vjust=0, hjust=0, color= "darkblue")+
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  labs(title = "Vaccinatie in Nederland",
        subtitle = "officiële cijfers",
      # subtitle = "Streefpercentage: 70%+",
       caption = paste("Bron: github.com/YorickBleijenberg | Plot: @YorickB | ",Sys.Date()))+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"),
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    legend.position = "none",   
    plot.title = element_text(hjust = 0.5,size = 40,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text=element_blank(),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.line = element_line(colour = "#F5F5F5")
    
  )

ggsave("data/93_vaccinated.png",width=16, height = 9)  

