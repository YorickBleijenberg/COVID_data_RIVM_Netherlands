



# Leeftijd opnames op Klinische afdeling
leeftijd.klinisch.week <- rjson::fromJSON(file = "https://stichting-nice.nl/covid-19/public/zkh/age-per-week/",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

leeftijd.klinisch.week <- as.data.frame(t(leeftijd.klinisch.week[c(1,2,4,6,8,10,12,14),]))


leeftijd.klinisch.week$V1 <- unlist(leeftijd.klinisch.week$V1)
leeftijd.klinisch.week$V2 <- unlist(leeftijd.klinisch.week$V2)
leeftijd.klinisch.week$V3 <- unlist(leeftijd.klinisch.week$V3)
leeftijd.klinisch.week$V4 <- unlist(leeftijd.klinisch.week$V4)
leeftijd.klinisch.week$V5 <- unlist(leeftijd.klinisch.week$V5)
leeftijd.klinisch.week$V6 <- unlist(leeftijd.klinisch.week$V6)
leeftijd.klinisch.week$V7 <- unlist(leeftijd.klinisch.week$V7)
leeftijd.klinisch.week$V8 <- unlist(leeftijd.klinisch.week$V8)

leeftijd.klinisch.week$sum <- (leeftijd.klinisch.week$V2 + leeftijd.klinisch.week$V3 +
                              leeftijd.klinisch.week$V4 + leeftijd.klinisch.week$V4 + leeftijd.klinisch.week$V6 +
                              leeftijd.klinisch.week$V7 + leeftijd.klinisch.week$V8)

colnames(leeftijd.klinisch.week) <- c("date","0-29","30-39","40-49", "50-59", "60-69", "70-79", "80+", "totaal")



library(wesanderson)


leeftijd.klinisch.week$date <- as.Date(leeftijd.klinisch.week$date)


key <- "date"
value <- "number.in.hosp"
leeftijd.klinisch.week.long <- gather(leeftijd.klinisch.week, key, value, 2:8)

#colnames (leeftijd.klinisch.week.long) <- c("date", "total" , "age", "number.in.hosp")


#### plot  #####


ggplot(leeftijd.klinisch.week.long, aes(x=date, y=value, fill = factor(key, levels=c("80+",
                                                                                              "70-79",
                                                                                              "60-69",
                                                                                              "50-59",
                                                                                              "40-49",
                                                                                              "30-39",
                                                                                              "0-29"))))+
  
  geom_bar(stat='identity')+
  scale_fill_manual(values = wes_palette("GrandBudapest1", 7, type = "continuous"))+ 
  
  scale_x_date( limits = c(as.Date("2021-06-02"), NA), breaks = "week",  labels = date_format("%V"))+
  
  labs(title = "Nieuwe opnames ziekenhuis per week",
       #subtitle = "met 7 daags voortschrijdend gemiddelde",
       caption = paste("Bron: NICE | Plot: @YorickB | ",Sys.Date()))+

  theme_classic()+
  xlab("")+ 
  ylab("")+

  theme(legend.position = c(0.05, 0.5),
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    #legend.position = "none",   # no legend
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14), 
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"))+
  
ggsave("data/plots/77_NICE_age_hosp_per_week.png",width=16, height = 9)

 

ggplot(leeftijd.klinisch.week.long, aes(x=date, y=value, fill = factor(key, levels=c("80+",
                                                                                     "70-79",
                                                                                     "60-69",
                                                                                     "50-59",
                                                                                     "40-49",
                                                                                     "30-39",
                                                                                     "0-29"))))+
  
  geom_bar(stat='identity', position=position_fill())+
  
  scale_fill_manual(values = wes_palette("GrandBudapest1", 7, type = "continuous"))+ 
  scale_x_date( limits = c(as.Date("2020-02-24"), NA), breaks = "2 week",  labels = date_format("%V"))+
  scale_y_continuous( label = percent_format(), sec.axis = sec_axis(~ . * 1, label = percent_format()))+
  
  
  labs(title = "Nieuwe opnames ziekenhuis per week",
       subtitle = "Relative verhouding tussen groepen onderling.",
       caption = paste("Bron: NICE | Plot: @YorickB | ",Sys.Date()))+

    theme_classic()+
  xlab("")+ 
  ylab("")+
  
  theme(legend.position = "right",
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
  
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    #legend.position = "none",   # no legend
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14), 
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))+
  
  ggsave("data/plots/77_NICE_age_hosp_per_week_rel.png",width=16, height = 9)













# Leeftijd opnames op Klinische afdeling
leeftijd.IC.week <- rjson::fromJSON(file = "https://stichting-nice.nl/covid-19/public/age-per-week/",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

leeftijd.IC.week <- as.data.frame(t(leeftijd.IC.week[c(1,2,4,6,8,10,12,14),]))


leeftijd.IC.week$V1 <- unlist(leeftijd.IC.week$V1)
leeftijd.IC.week$V2 <- unlist(leeftijd.IC.week$V2)
leeftijd.IC.week$V3 <- unlist(leeftijd.IC.week$V3)
leeftijd.IC.week$V4 <- unlist(leeftijd.IC.week$V4)
leeftijd.IC.week$V5 <- unlist(leeftijd.IC.week$V5)
leeftijd.IC.week$V6 <- unlist(leeftijd.IC.week$V6)
leeftijd.IC.week$V7 <- unlist(leeftijd.IC.week$V7)
leeftijd.IC.week$V8 <- unlist(leeftijd.IC.week$V8)

leeftijd.IC.week$sum <- (leeftijd.IC.week$V2 + leeftijd.IC.week$V3 +
                           leeftijd.IC.week$V4 + leeftijd.IC.week$V4 + leeftijd.IC.week$V6 +
                           leeftijd.IC.week$V7 + leeftijd.IC.week$V8)

colnames(leeftijd.IC.week) <- c("date","0-29","30-39","40-49", "50-59", "60-69", "70-79", "80+", "totaal")



library(wesanderson)


leeftijd.IC.week$date <- as.Date(leeftijd.IC.week$date)


key <- "date"
value <- "number.in.hosp"
leeftijd.IC.week.long <- gather(leeftijd.IC.week, key, value, 2:8)

#colnames (leeftijd.klinisch.week.long) <- c("date", "total" , "age", "number.in.hosp")


#### plot  #####


ggplot(leeftijd.IC.week.long, aes(x=date, y=value, fill = factor(key, levels=c("80+",
                                                                                     "70-79",
                                                                                     "60-69",
                                                                                     "50-59",
                                                                                     "40-49",
                                                                                     "30-39",
                                                                                     "0-29"))))+
  
  geom_bar(stat='identity')+
  scale_fill_manual(values = wes_palette("Darjeeling2", 7, type = "continuous"))+ 
  
  scale_x_date( limits = c(as.Date("2021-06-02"), NA), breaks = "week",  labels = date_format("%V"))+
  
  
  
  labs(title = "Nieuwe opnames IC per week",
       #subtitle = "met 7 daags voortschrijdend gemiddelde",
       caption = paste("Bron: NICE | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
  theme(legend.position = c(0.05, 0.5),
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    #legend.position = "none",   # no legend
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14), 
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"))+
  
  ggsave("data/plots/77_NICE_age_IC_per_week.png",width=16, height = 9)



ggplot(leeftijd.IC.week.long, aes(x=date, y=value, fill = factor(key, levels=c("80+",
                                                                                     "70-79",
                                                                                     "60-69",
                                                                                     "50-59",
                                                                                     "40-49",
                                                                                     "30-39",
                                                                                     "0-29"))))+
  
  geom_bar(stat='identity', position=position_fill())+
  
  scale_fill_manual(values = wes_palette("Darjeeling2", 7, type = "continuous"))+ 
  scale_x_date( limits = c(as.Date("2020-02-24"), NA), breaks = "2 week",  labels = date_format("%V"))+
  scale_y_continuous( label = percent_format(), sec.axis = sec_axis(~ . * 1, label = percent_format()))+
  
  
  labs(title = "Nieuwe opnames IC per week",
       subtitle = "Relative verhouding tussen groepen onderling.",
       caption = paste("Bron: NICE | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
  theme(legend.position = "right",
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
  
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    #legend.position = "none",   # no legend
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14), 
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))+
  
  ggsave("data/plots/77_NICE_age_IC_per_week_rel.png",width=16, height = 9)











