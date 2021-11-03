




casus.age.dif.abs <- cassus.age.spread.base

casus.age.dif.abs$date <- as.Date(casus.age.dif.abs$date)
casus.age.dif.abs$`0-9`     <- as.integer(casus.age.dif.abs$`0-9` )
casus.age.dif.abs$`10-19`   <- as.integer(casus.age.dif.abs$`10-19`)
casus.age.dif.abs$`20-29`   <- as.integer(casus.age.dif.abs$`20-29` )
casus.age.dif.abs$`30-39`   <- as.integer(casus.age.dif.abs$`30-39` )
casus.age.dif.abs$`40-49`   <- as.integer(casus.age.dif.abs$`40-49`)
casus.age.dif.abs$`50-59`   <- as.integer(casus.age.dif.abs$`50-59`)
casus.age.dif.abs$`60-69`   <- as.integer(casus.age.dif.abs$`60-69`)
casus.age.dif.abs$`70-79`   <- as.integer(casus.age.dif.abs$`70-79`)
casus.age.dif.abs$`80-89`   <- as.integer(casus.age.dif.abs$`80-89`)
casus.age.dif.abs$`90+`    <- as.integer(casus.age.dif.abs$`90+`)


casus.age.dif.abs$`MA.0-9`    <- rollmeanr((casus.age.dif.abs$`0-9`  )   , 7, fill = 0)
casus.age.dif.abs$`MA.10-19`  <- rollmeanr((casus.age.dif.abs$`10-19`)  , 7, fill = 0)
casus.age.dif.abs$`MA.20-29`  <- rollmeanr((casus.age.dif.abs$`20-29`)  , 7, fill = 0)
casus.age.dif.abs$`MA.30-39`  <- rollmeanr((casus.age.dif.abs$`30-39`)  , 7, fill = 0)
casus.age.dif.abs$`MA.40-49`  <- rollmeanr((casus.age.dif.abs$`40-49`)  , 7, fill = 0)
casus.age.dif.abs$`MA.50-59`  <- rollmeanr((casus.age.dif.abs$`50-59`)  , 7, fill = 0)
casus.age.dif.abs$`MA.60-69`  <- rollmeanr((casus.age.dif.abs$`60-69`)  , 7, fill = 0)
casus.age.dif.abs$`MA.70-79`  <- rollmeanr((casus.age.dif.abs$`70-79`)  , 7, fill = 0)
casus.age.dif.abs$`MA.80-89`  <- rollmeanr((casus.age.dif.abs$`80-89`)  , 7, fill = 0)
casus.age.dif.abs$`MA.90+`    <- rollmeanr((casus.age.dif.abs$`90+`  )  , 7, fill = 0)



#relative.casus.age.dif.abs <- casus.age.dif.abs[ -c(1:11,13:23)]
casus.age.dif.abs <- casus.age.dif.abs[casus.age.dif.abs$date > "2021-05-26",]  #"2021-06-08",]
casus.age.dif.abs.short <- casus.age.dif.abs[casus.age.dif.abs$date >"2021-02-16",]




casus.age.dif.abs.test <- casus.age.dif.abs[,-(1:11)]   
casus.age.dif.abs.test <- casus.age.dif.abs.test[,-(2)]   

key <- "date"
value <- "vacc_total"
#gathercols <- c("MA.0-9","MA.10-19","MA.20-29", "MA.30-39")
casus.age.dif.abs.long <- gather(casus.age.dif.abs.test, key, value, (2:11))


# `MA.0-9` 
# `MA.10-19`
# `MA.20-29`
# `MA.30-39`
# `MA.40-49`
# `MA.50-59`
# `MA.60-69`
# `MA.70-79`
# `MA.80-89`
# `MA.90+` 




ggplot(casus.age.dif.abs.long)+
  
  geom_line(  aes(x= date, y=value, color = key),  lwd=2.5)+
  
  #scale_color_discrete(safe_colorblind_palette)
  theme_classic()+
  
  xlab("")+
  ylab("")+
  
  scale_x_date(date_breaks = "1 week", 
               date_labels= format("%d %b"),
               limits = as.Date(c("2021-08-01", NA)))+
  scale_y_continuous(limits = c(NA, 1500))+
  
  scale_color_brewer(palette = "RdYlBu", labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+"))+
  # scale_colour_viridis_d(option = "cividis")+
  
  theme(#legend.position = "none",   # no legend
    legend.title       = element_blank(),  ## legend title
    legend.position    = "right",
    legend.direction   = "vertical",
    legend.background  = element_rect(fill="darkgrey", size=0.5, linetype="solid"),
    legend.text = element_text(colour="black", size=22, face="bold"))+
  
  labs(title     = "Besmettingen, absolute aantallen.",
       subtitle = "verschil tussen casusfiles \n 7-daags lopende gemiddelden",  #subtitle.label,
       caption   = paste("Bron: RIVM | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(
    plot.background    = element_rect(fill = "darkgrey"  ), ###"#F5F5F5"), #background color/size (border color and size)
    panel.background   = element_rect(fill = "darkgrey", colour = "darkgrey"),
    plot.title         = element_text(hjust = 0.5 ,size = 40 ,face = "bold"),
    plot.subtitle      = element_text(hjust=0.5   ,size = 15 ,color = "black", face = "italic"),
    axis.text          = element_text(size=14,color = "black",face = "bold"),
    axis.ticks         = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length  = unit(0.5, "cm"),
    axis.line          = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))

ggsave("data/plots/99_leeftijd_case_abs_short.png",width=16, height = 9)










#### other #####







ggplot(casus.age.dif.abs.long)+
  
  geom_line(  aes(x= date, y=value, color = key),  lwd=2.5)+
 
  #scale_color_discrete(safe_colorblind_palette)
  theme_classic()+
  
  xlab("")+
  ylab("")+
  
 # scale_x_date(date_breaks = "1 week", 
 #              date_labels= format("%d %b"),
 #              limits = as.Date(c("2021-08-01", NA)))+
 # scale_y_continuous(limits = c(NA, 40))+
  
  scale_color_brewer(palette = "RdYlBu", labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+"))+
 # scale_colour_viridis_d(option = "cividis")+
  
  theme(#legend.position = "none",   # no legend
    legend.title       = element_blank(),  ## legend title
    legend.position    = "right",
    legend.direction   = "vertical",
    legend.background  = element_rect(fill="darkgrey", size=0.5, linetype="solid"),
    legend.text = element_text(colour="black", size=22, face="bold"))+
  
  labs(title     = "Besmettingen",
       subtitle = "verschil tussen casusfiles \n 7-daags lopende gemiddelden",  #subtitle.label,
       caption   = paste("Bron: RIVM | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(
    plot.background    = element_rect(fill = "darkgrey"  ), ###"#F5F5F5"), #background color/size (border color and size)
    panel.background   = element_rect(fill = "darkgrey", colour = "darkgrey"),
    plot.title         = element_text(hjust = 0.5 ,size = 40 ,face = "bold"),
    plot.subtitle      = element_text(hjust=0.5   ,size = 15 ,color = "black", face = "italic"),
    axis.text          = element_text(size=14,color = "black",face = "bold"),
    axis.ticks         = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length  = unit(0.5, "cm"),
    axis.line          = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))
# ggsave("data/plots/99_leeftijd_case_abs.png",width=16, height = 9)
  





ggplot(casus.age.dif.abs.long)+
  
  geom_line(  aes(x= date, y=value, color = key),  lwd=2.5)+
  
  #scale_color_discrete(safe_colorblind_palette)
  theme_classic()+
  
  xlab("")+
  ylab("")+
  
  #scale_x_date(date_breaks = "1 week", 
  #             date_labels= format("%d %b"),
  #             limits = as.Date(c("2021-08-01", NA)))+
  scale_y_continuous(trans='log2', breaks = c(2,4,8,16,32,64,128,256,512,1024,2048,4096))+
  
  scale_color_brewer(palette = "RdYlBu", labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+"))+
  # scale_colour_viridis_d(option = "cividis")+
  
  theme(#legend.position = "none",   # no legend
    legend.title       = element_blank(),  ## legend title
    legend.position    = "right",
    legend.direction   = "vertical",
    legend.background  = element_rect(fill="darkgrey", size=0.5, linetype="solid"),
    legend.text = element_text(colour="black", size=22, face="bold"))+
  
  labs(title     = "Besmettingen, absoluut, logaritmisch",
       subtitle = "verschil tussen casusfiles \n 7-daags lopende gemiddelden",  #subtitle.label,
       caption   = paste("Bron: RIVM | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(
    plot.background    = element_rect(fill = "darkgrey"  ), ###"#F5F5F5"), #background color/size (border color and size)
    panel.background   = element_rect(fill = "darkgrey", colour = "darkgrey"),
    plot.title         = element_text(hjust = 0.5 ,size = 40 ,face = "bold"),
    plot.subtitle      = element_text(hjust=0.5   ,size = 15 ,color = "black", face = "italic"),
    axis.text          = element_text(size=14,color = "black",face = "bold"),
    axis.ticks         = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length  = unit(0.5, "cm"),
    axis.line          = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))
ggsave("data/plots/99_leeftijd_case_abs_log.png",width=16, height = 9)



