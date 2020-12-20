



diff.calc <- Merged_data_2

#diff.calc$diff_perc_c  <- ((diff.calc$cases/ diff.calc$MACases)-1)*100
#diff.calc$diff_perc_h  <- ((diff.calc$hosp/ diff.calc$MAhosp)-1)*100
#diff.calc$diff_perc_d  <- ((diff.calc$dead/ diff.calc$MAdead)-1)*100

diff.calc$diff_perc_c  <- ((diff.calc$cases/ diff.calc$ma_c_lead)-1)*100
diff.calc$diff_perc_h  <- ((diff.calc$hosp/ diff.calc$ma_h_lead)-1)*100
diff.calc$diff_perc_d  <- ((diff.calc$dead/ diff.calc$ma_d_lead)-1)*100

diff.calc.sh  <- diff.calc[ -c(2:16)]
diff.calc.sh <- diff.calc.sh[diff.calc.sh$dateInTable>"2020-09-10"&diff.calc.sh$dateInTable<=Sys.Date(),]

diff.calc.sh$dateInTable <- as.Date(diff.calc.sh$dateInTable)
diff.calc.sh$week_day <- weekdays(diff.calc.sh$dateInTable)

diff.calc.sh$week_day <- str_replace_all(diff.calc.sh$week_day, "Monday", "maandag")
diff.calc.sh$week_day <- str_replace_all(diff.calc.sh$week_day, "Tuesday", "dinsdag")
diff.calc.sh$week_day <- str_replace_all(diff.calc.sh$week_day, "Wednesday", "woensdag")
diff.calc.sh$week_day <- str_replace_all(diff.calc.sh$week_day, "Thursday", "donderdag")
diff.calc.sh$week_day <- str_replace_all(diff.calc.sh$week_day, "Friday", "vrijdag")
diff.calc.sh$week_day <- str_replace_all(diff.calc.sh$week_day, "Saturday", "zaterdag")
diff.calc.sh$week_day <- str_replace_all(diff.calc.sh$week_day, "Sunday", "zondag")

diff.calc.sh$week_day <- as.factor(diff.calc.sh$week_day)
diff.calc.sh$week_day = factor(diff.calc.sh$week_day,levels = c("maandag","dinsdag","woensdag","donderdag", "vrijdag", "zaterdag","zondag"))

diff.calc.sh <- diff.calc.sh[-nrow(diff.calc.sh),]
diff.calc.sh <- diff.calc.sh[-nrow(diff.calc.sh),]
diff.calc.sh <- diff.calc.sh[-nrow(diff.calc.sh),]

diff.calc.mean <- aggregate(diff.calc.sh[, 2:4], list(diff.calc.sh$week_day), mean)

diff.calc.mean$Besmettingen <- round(diff.calc.mean$diff_perc_c, digits = 1)
diff.calc.mean$Opnames <- round(diff.calc.mean$diff_perc_h, digits = 1)
diff.calc.mean$Overleden <- round(diff.calc.mean$diff_perc_d, digits = 1)

key <- "DateInTable"
value <- "percentage"
#gathercols <- c("Besmettingen","Opnames","Overleden")
gathercols <- c("Besmettingen","Overleden")
diff.calc.mean.wide <- gather(diff.calc.mean, key, value, all_of(gathercols))






ggplot(diff.calc.mean.wide)+
  geom_bar(stat='identity', aes(x=Group.1, y=value, fill = key))+
  scale_fill_manual(values = c("darkblue", "darkred"))+
  
  geom_hline(yintercept=0)+
  
  
  
  facet_wrap(~key, ncol = 1, scales = "free_y")+
  
  theme_bw() + 
  
  scale_y_continuous(labels = label_percent(scale = 1))+

  
  xlab("")+ 
  ylab("")+
  labs(title = "Besmettingen en doden, afwijking van het 7-daags gemiddelde per dag",
       subtitle = "vanaf 1 september 2020",
       caption = paste("bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         legend.position = "none",   # no legend
         
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=12),  #, angle=45),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
         
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
         
         ### facet label custom
         strip.text.x = element_text(size = 13, color = "black"),
         strip.background = element_rect(color="black", fill="gray", size=1.5, linetype="solid"),
         
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
  )
ggsave("data/85_weekeffects_september.png",width=16, height = 9)  
