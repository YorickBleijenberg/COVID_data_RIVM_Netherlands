




casus.age.dif.play <- cassus.age.spread.base  # read.csv("C:\\Rdir\\data-contstant\\casus_daily_age_dif.csv" ,sep=";", check.names = FALSE)

casus.age.dif.play$date <- as.Date(casus.age.dif.play$date)
casus.age.dif.play$`0-9`     <- as.integer(casus.age.dif.play$`0-9` )
casus.age.dif.play$`10-19`   <- as.integer(casus.age.dif.play$`10-19`)
casus.age.dif.play$`20-29`   <- as.integer(casus.age.dif.play$`20-29` )
casus.age.dif.play$`30-39`   <- as.integer(casus.age.dif.play$`30-39` )
casus.age.dif.play$`40-49`   <- as.integer(casus.age.dif.play$`40-49`)
casus.age.dif.play$`50-59`   <- as.integer(casus.age.dif.play$`50-59`)
casus.age.dif.play$`60-69`   <- as.integer(casus.age.dif.play$`60-69`)
casus.age.dif.play$`70-79`   <- as.integer(casus.age.dif.play$`70-79`)
casus.age.dif.play$`80-89`   <- as.integer(casus.age.dif.play$`80-89`)
casus.age.dif.play$`90+`    <- as.integer(casus.age.dif.play$`90+`)


casus.age.dif.play$`MA.0-9`    <- rollmeanr((casus.age.dif.play$`0-9` *100000/(1755525))   , 7, fill = 0)
casus.age.dif.play$`MA.10-19`  <- rollmeanr((casus.age.dif.play$`10-19`*100000/(1983199))  , 7, fill = 0)
casus.age.dif.play$`MA.20-29`  <- rollmeanr((casus.age.dif.play$`20-29`*100000/(2240325))  , 7, fill = 0)
casus.age.dif.play$`MA.30-39`  <- rollmeanr((casus.age.dif.play$`30-39`*100000/(2184528))  , 7, fill = 0)
casus.age.dif.play$`MA.40-49`  <- rollmeanr((casus.age.dif.play$`40-49`*100000/(2162516))  , 7, fill = 0)
casus.age.dif.play$`MA.50-59`  <- rollmeanr((casus.age.dif.play$`50-59`*100000/(2550020))  , 7, fill = 0)
casus.age.dif.play$`MA.60-69`  <- rollmeanr((casus.age.dif.play$`60-69`*100000/(2143755))  , 7, fill = 0)
casus.age.dif.play$`MA.70-79`  <- rollmeanr((casus.age.dif.play$`70-79`*100000/(1618615))  , 7, fill = 0)
casus.age.dif.play$`MA.80-89`  <- rollmeanr((casus.age.dif.play$`80-89`*100000/(706413))  , 7, fill = 0)
casus.age.dif.play$`MA.90+`    <- rollmeanr((casus.age.dif.play$`90+`  *100000/(131012))  , 7, fill = 0)



#relative.casus.age.dif.play <- casus.age.dif.play[ -c(1:11,13:23)]
casus.age.dif.play <- casus.age.dif.play[casus.age.dif.play$date > "2021-05-26",]  #"2021-06-08",]
casus.age.dif.play.short <- casus.age.dif.play[casus.age.dif.play$date >"2021-02-16",]




casus.age.dif.play.test <- casus.age.dif.play[,-(1:11)]   
casus.age.dif.play.test <- casus.age.dif.play.test[,-(2)]   

key <- "date"
value <- "vacc_total"
#gathercols <- c("MA.0-9","MA.10-19","MA.20-29", "MA.30-39")
casus.age.dif.play.long <- gather(casus.age.dif.play.test, key, value, (2:11))


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



ggplot(casus.age.dif.play.long)+
  
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
    legend.background  = element_rect(fill="#cccccc", size=0.5, linetype="solid"),
    legend.text = element_text(colour="black", size=22, face="bold"))+
  
  labs(title     = "Besmettingen, per 100k",
       subtitle = "verschil tussen casusfiles \n 7-daags lopende gemiddelden",  #subtitle.label,
       caption   = paste("Bron: RIVM | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(
    plot.background    = element_rect(fill = "#cccccc"  ), ###"#F5F5F5"), #background color/size (border color and size)
    panel.background   = element_rect(fill = "#cccccc", colour = "#cccccc"),
    plot.title         = element_text(hjust = 0.5 ,size = 40 ,face = "bold"),
    plot.subtitle      = element_text(hjust=0.5   ,size = 15 ,color = "black", face = "italic"),
    axis.text          = element_text(size=14,color = "black",face = "bold"),
    axis.ticks         = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length  = unit(0.5, "cm"),
    axis.line          = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "darkgray", linetype = "dashed"))
# ggsave("data/plots/99_leeftijd_case_phd.png",width=16, height = 9)
  


ggplot(casus.age.dif.play.long)+
  
  geom_line(  aes(x= date, y=value, color = key),  lwd=2.5)+
  
  #scale_color_discrete(safe_colorblind_palette)
  theme_classic()+
  
  xlab("")+
  ylab("")+
  
   scale_x_date(date_breaks = "1 week", 
                date_labels= format("%d %b"),
                limits = as.Date(c("2021-08-01", NA)))+
   scale_y_continuous(limits = c(NA, 40))+
  
  scale_color_brewer(palette = "RdYlBu", labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+"))+
  # scale_colour_viridis_d(option = "cividis")+
  
  theme(#legend.position = "none",   # no legend
    legend.title       = element_blank(),  ## legend title
    legend.position    = "right",
    legend.direction   = "vertical",
    legend.background  = element_rect(fill="#cccccc", size=0.5, linetype="solid"),
    legend.text = element_text(colour="black", size=22, face="bold"))+
  
  labs(title     = "Besmettingen, per 100k",
       subtitle = "Verschil tussen casusfiles \n 7-daags lopende gemiddelden",  #subtitle.label,
       caption   = paste("Bron: RIVM | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(
    plot.background    = element_rect(fill = "#cccccc"  ), ###"#F5F5F5"), #background color/size (border color and size)
    panel.background   = element_rect(fill = "#cccccc", colour = "#cccccc"),
    plot.title         = element_text(hjust = 0.5 ,size = 40 ,face = "bold"),
    plot.subtitle      = element_text(hjust=0.5   ,size = 15 ,color = "black", face = "italic"),
    axis.text          = element_text(size=14,color = "black",face = "bold"),
    axis.ticks         = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length  = unit(0.5, "cm"),
    axis.line          = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "darkgray", linetype = "dashed"))+
  ggsave("data/plots/99_leeftijd_case_phd_short.png",width=16, height = 9)



ggplot(casus.age.dif.play.long)+
  
  geom_line(  aes(x= date, y=value, color = key),  lwd=2.5)+
  
  #scale_color_discrete(safe_colorblind_palette)
  theme_classic()+
  
  xlab("")+
  ylab("")+
  
  #scale_x_date(date_breaks = "1 week", 
  #             date_labels= format("%d %b"),
  #             limits = as.Date(c("2021-08-01", NA)))+
  scale_y_continuous(trans='log2', breaks = c(1,2,4,8,16,32,64,128,256))+
  
  scale_color_brewer(palette = "RdYlBu", labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+"))+
  # scale_colour_viridis_d(option = "cividis")+
  
  theme(#legend.position = "none",   # no legend
    legend.title       = element_blank(),  ## legend title
    legend.position    = "right",
    legend.direction   = "vertical",
    legend.background  = element_rect(fill="#cccccc", size=0.5, linetype="solid"),
    legend.text = element_text(colour="black", size=22, face="bold"))+
  
  labs(title     = "Besmettingen, per 100k, logaritmisch",
       subtitle = "verschil tussen casusfiles \n 7-daags lopende gemiddelden",  #subtitle.label,
       caption   = paste("Bron: RIVM | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(
    plot.background    = element_rect(fill = "#cccccc"  ), ###"#F5F5F5"), #background color/size (border color and size)
    panel.background   = element_rect(fill = "#cccccc", colour = "#cccccc"),
    plot.title         = element_text(hjust = 0.5 ,size = 40 ,face = "bold"),
    plot.subtitle      = element_text(hjust=0.5   ,size = 15 ,color = "black", face = "italic"),
    axis.text          = element_text(size=14,color = "black",face = "bold"),
    axis.ticks         = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length  = unit(0.5, "cm"),
    axis.line          = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "darkgray", linetype = "dashed"))+
  ggsave("data/plots/99_leeftijd_case_phd_log.png",width=16, height = 9)



