
b117_read <-read.csv("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/new_variants/b117.csv",sep=",")
b117_read$date <- as.Date(b117_read$date)

today <- Sys.Date()

ggplot(b117_read)+
  #geom_density(aes(x=date, y=b117_total), fill = "darkred")+
  geom_smooth(aes(x=date, y=b117_total),method = "lm", formula = y ~ poly(x, 5), fill = "black", se = FALSE, size=3)+
  
  geom_density(aes(x=date),alpha=.2, fill="#FF6666") +
  
  
  scale_x_date(date_breaks = "1 weeks", 
               date_labels= format("%d/%m"),
               limits = as.Date(c("2020-12-01", today)))+
  scale_y_continuous(limits = c(0, 250), labels = label_comma(big.mark = ".", decimal.mark = ","))+  #breaks = c(2500, 5000, 7500,10000,12500,15000),
  coord_cartesian(expand = FALSE)+
  theme_classic()+
  xlab("")+
  ylab("")+
  
  labs(title = "B.1.1.7 - VOC 202012/01 in the Netherlands",
       subtitle = "De groei lijkt wel exponentieel",
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
  
  ggsave("data/93_B117_tot.png",width=16, height = 9)  



ggplot(b117_read)+
  geom_col(aes(x=date, y=b117_total), fill = "darkred")+

  scale_x_date(date_breaks = "1 weeks", 
               date_labels= format("%d/%m"),
               limits = as.Date(c("2020-12-01", today)))+
  scale_y_continuous(limits = c(0, 250), labels = label_comma(big.mark = ".", decimal.mark = ","))+  #breaks = c(2500, 5000, 7500,10000,12500,15000),
  coord_cartesian(expand = FALSE)+
  theme_classic()+
  xlab("")+
  ylab("")+
  
  labs(title = "B.1.1.7 - VOC 202012/01 in the Netherlands",
       subtitle = "De groei lijkt wel exponentieel",
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
  
  ggsave("data/93_B117_tot_bar.png",width=16, height = 9)  


