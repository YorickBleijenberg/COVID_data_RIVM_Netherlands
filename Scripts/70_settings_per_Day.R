

#### settings per day #####

library(wesanderson)


settings_json <- "https://data.rivm.nl/covid-19/COVID-19_aantallen_settings_per_dag.json"
settings_dat <- fromJSON(txt = settings_json)
settings_dat$Date_of_publication <- as.Date(settings_dat$Date_of_publication)
settings_dat <-  (settings_dat %>% filter(Number_settings_reported > 0 ))
settings_dat$Setting_reported <- as.factor(settings_dat$Setting_reported)


ggplot(settings_dat, aes(x=Date_of_publication, y=Number_settings_reported, fill = Setting_reported))+
#geom_col(position=position_fill())+
  geom_col()+
   scale_fill_manual(values = wes_palette("Darjeeling1", 21, type = "continuous"))+ 
  
ggsave("data/plots/lol_settings.png",width=16, height = 9)



ggplot(settings_dat, aes(x=Date_of_publication, y=Number_settings_reported, fill = Setting_reported))+
  #geom_col(position=position_fill())+
  geom_col()+
  facet_wrap(~Setting_reported,   scales = "free_y")+
  #scale_fill_manual(values=wes_palette(n=21, name="GrandBudapest"))
  
  scale_fill_manual(values = wes_palette("Darjeeling1", 21, type = "continuous"))+ 
  
  theme_bw() + 
  xlab("")+ 
  ylab("")+
  
  labs(title = "Settings",
       subtitle = "(Y-as wisselt)",
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         legend.position = "none",   # no legend
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         
         axis.text = element_text(size=6,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=6),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.2, "cm"),
         #axis.line = element_line(colour = "#F5F5F5"),
         
         #panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
         ### facet label custom
         strip.text.x = element_text(size = 5, color = "black"),
         strip.background = element_rect(fill="gray"),  #, color="black"), #, size=0.2, linetype="solid"),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.y = element_blank(),
         panel.grid.minor.y = element_blank(),
  )+

  
ggsave("data/plots/lol_settings_face.png",width=16, height = 9)
