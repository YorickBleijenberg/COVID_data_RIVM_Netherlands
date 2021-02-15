library(tidyverse)
library(gganimate)
library(transformr)

library(jsonlite)
library(lubridate)

library(zoo)
require(data.table)

pos.rate.x <- paste0("C:\\Rdir\\rivm-dashboard\\VR\\VR_", format((Sys.Date()), "%Y-%m-%d"), ".csv")
pos.rate <- read.csv(pos.rate.x,sep=";")  
pos.rate$week_unix <- isoweek(as.Date(as.POSIXct(pos.rate$week_unix, origin="1970-01-01"), format = "%V"))

tested_daily.toedit <-tested_daily
tested_daily.toedit <- tested_daily.toedit[ -c(1,2,5,7)]

colnames(tested_daily.toedit) = c("cases", "perc_pos", "date")


tested_daily.toedit$forteend <- (rollsum(tested_daily.toedit$cases, 14, fill = 0, align = "right"))
tested_daily.toedit$vdphd <- tested_daily.toedit$forteend/17474677*100000

#pos.rate.vr$date <- as.Date(pos.rate.vr$date)
#pos.rate.nl <- pos.rate.vr[ -c(5,6)]
#pos.rate.nl.2 <- aggregate(pos.rate.nl$cases, by=list(pos.rate.nl$week), FUN=sum)
#pos.rate.nl.3 <- aggregate(pos.rate.nl$tested, by=list(pos.rate.nl$week), FUN=sum)

#pos.rate.nl <- merge(pos.rate.nl.3,pos.rate.nl.2, by="Group.1")
#colnames(pos.rate.nl) = c("date","tested", "cases")

#pos.rate.nl$p.rate <- as.double(pos.rate.nl$cases / pos.rate.nl$tested *100)
#pos.rate.nl$MAcase <- (rollmeanr(pos.rate.nl$cases, 2, fill = 0))*2
#pos.rate.nl$vdphd <- pos.rate.nl$MAcase/17461543*100000


animation.thing <- ggplot(tested_daily.toedit, aes(perc_pos, vdphd, colour=date))+
  
  annotate("rect", xmin = 0, xmax = 4, ymin = 0, ymax = 25,color = "black", fill = "green", alpha = 0.75)+
  annotate("rect", xmin = 4, xmax = 22, ymin = 0, ymax = 25,color = "black", fill = "orange", alpha = 0.5)+
  
  annotate("rect", xmin = 0, xmax =4, ymin = 25, ymax = 50, color = "black", fill = "orange", alpha = 0.5)+
  annotate("rect", xmin = 4, xmax =22, ymin = 25, ymax = 50,color = "black", fill = "orange", alpha = 0.9)+
   
  annotate("rect", xmin = 0, xmax =4, ymin = 50, ymax = 150, color = "black",fill = "orange", alpha = 0.9)+
  annotate("rect", xmin = 4, xmax =22, ymin = 50, ymax = 150, color = "black",fill = "red", alpha = 0.4)+
  
  annotate("rect", xmin = 0, xmax =4, ymin = 150, ymax = 725, color = "black",fill = "red", alpha = 0.4)+
  annotate("rect", xmin = 4, xmax =22, ymin =150, ymax = 725, color = "black",fill = "red", alpha = 0.6)+
  
  #geom_line()+
  #geom_path(   size =0.5, arrow = arrow(angle = 20,type = "open"))+
 # geom_point( color = "black", size =4)+
 geom_point(   size =3)+

  scale_x_continuous(labels = scales::percent_format(scale = 1,accuracy = 1))+
  scale_color_gradient(low="gray", high="blue")+

  theme_classic()+
   xlab("Percentage positieve testen")+ 
   ylab("Aantal nieuwe gevallen per 100.000, per 14 dagen")+
 
  labs(title = "ECDC check",
       subtitle = "Nieuwe gevallen per 100.000, per 14 dagen  VS  percentage positieve testen \n -keurcodering volgens ECDC richtlijn-",
       caption = paste("Bron: ECDC / RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         legend.position = "none",   # no legend
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=12),  #, angle=45),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
         axis.line = element_line(colour = "#F5F5F5"),
         
         panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"),
         ### facet label custom
         strip.text.x = element_text(size = 13, color = "black"),
         strip.background = element_rect(color="black", fill="gray", size=1.5, linetype="solid"))+

  transition_time(date) +
  labs(title = "Datum: {frame_time}") +
  view_follow(fixed_y = TRUE)+
 shadow_wake(wake_length = 0.0, alpha = FALSE)

  animate(animation.thing, fps=4)

  
anim_save("data/60_ECDC-5.gif",width=16, height = 9)  

  #####
  
last.point <- last(tested_daily.toedit)


ggplot(tested_daily.toedit, aes(perc_pos, vdphd, colour=date))+
  
  annotate("rect", xmin = 0, xmax = 4, ymin = 0, ymax = 25,color = "black", fill = "green", alpha = 0.75)+
  annotate("rect", xmin = 4, xmax = 22, ymin = 0, ymax = 25,color = "black", fill = "orange", alpha = 0.5)+
  
  annotate("rect", xmin = 0, xmax =4, ymin = 25, ymax = 50, color = "black", fill = "orange", alpha = 0.5)+
  annotate("rect", xmin = 4, xmax =22, ymin = 25, ymax = 50,color = "black", fill = "orange", alpha = 0.9)+
  
  annotate("rect", xmin = 0, xmax =4, ymin = 50, ymax = 150, color = "black",fill = "orange", alpha = 0.9)+
  annotate("rect", xmin = 4, xmax =22, ymin = 50, ymax = 150, color = "black",fill = "red", alpha = 0.4)+
  
  annotate("rect", xmin = 0, xmax =4, ymin = 150, ymax = 725, color = "black",fill = "red", alpha = 0.4)+
  annotate("rect", xmin = 4, xmax =22, ymin =150, ymax = 725, color = "black",fill = "red", alpha = 0.6)+
  
  geom_path(   size =0.5, arrow = arrow(angle = 20,type = "open"), color = "black")+
  geom_point( color = "black", size =4)+
  geom_point(   size =3)+
  
 geom_point(data=last.point, color="green")+
  
  scale_x_continuous(labels = scales::percent_format(scale = 1,accuracy = 1))+
  scale_color_gradient(low="gray", high="darkblue")+
  
  theme_classic()+
  xlab("Percentage positieve testen")+ 
  ylab("Aantal nieuwe gevallen per 100.000, per 14 dagen")+
  
  labs(title = "ECDC check",
       subtitle = "Nieuwe gevallen per 100.000, per 14 dagen  VS  percentage positieve testen \n -keurcodering volgens ECDC richtlijn-",
       caption = paste("Bron: ECDC / RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         legend.position = "none",   # no legend
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=12),  #, angle=45),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
         axis.line = element_line(colour = "#F5F5F5"),
         
         panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"),
         ### facet label custom
         strip.text.x = element_text(size = 13, color = "black"),
         strip.background = element_rect(color="black", fill="gray", size=1.5, linetype="solid"))
  


ggsave("data/63_ECDC_6.png", width = 16, height = 9)  


  
  #####
  
  #####
  
  
  pos.rate <- paste0("C:\\Rdir\\rivm-dashboard\\VR\\VR_", format((Sys.Date()), "%Y-%m-%d"), ".csv")
  pos.rate <- read.csv(pos.rate,sep=";")  
  
  pos.rate.vr <- pos.rate[ -c(1,3,4,8)]
  colnames(pos.rate.vr) = c("week", "cases", "perc_pos","tested", "vregio", "date" )
  pos.rate.vr$date <- as.Date(pos.rate.vr$date)
  
  vr <- "C:\\Rdir\\data-contstant\\veiligheidsregios.csv"
  VR <- read.csv(vr,sep=";")  
  colnames(VR) = c("vrcode", "vregio", "inwoners")
  
  FR_a <- intToUtf8(0x00E2)  #Encoding(FR_b) <- "UTF-8"
  FR_b <- paste0("Frysl", FR_a,"n")
  
  VR$vregio <- str_replace(VR$vregio, "Fryslân", FR_b)  ##fout / goed
  VR$vregio <- as.factor(VR$vregio)
  
  VR$inwoners <- as.integer(VR$inwoners)
  pos.rate.vr$week2 <- isoweek(as.Date(as.POSIXct(pos.rate.vr$week, origin="1970-01-01"), format = "%V"))
  
  VR <- VR[ -c(1,4,5)]
  pos.rate.vr <- pos.rate.vr[ -c(1,3,6:9)]

  VR_2 <- merge(VR,pos.rate.vr) #,"vregio")
  
  VR_2$MAcase <- (rollmeanr(VR_2$cases, 2, fill = 0))*2
  VR_2 <- VR_2[VR_2$week2>40 & VR_2$week2<= 53,]                       ######   week
  
  VR_2$posphd <- as.double(VR_2$MAcase / VR_2$inwoners*100000)
  VR_2$p.pos <- as.double(VR_2$cases / VR_2$tested*100)
  #VR_2 <- VR_2[ -c(2,4,5,6,7,8)]
  
  VR_2$perc_pos <- as.numeric(VR_2$perc_pos)
  VR_3 <- VR_2[VR_2$week2>41 & VR_2$week2<= 53,]                     ######   week
  
  
  ggplot(VR_3, aes(x = p.pos, y=posphd))+ #  , colour = vregio))+
    annotate("rect", xmin = 0, xmax = 4, ymin = 0, ymax = 25, fill = "green", alpha = 0.75)+
    annotate("rect", xmin = 4, xmax = 27, ymin = 0, ymax = 25, fill = "orange", alpha = 0.5)+
    
    annotate("rect", xmin = 0, xmax =4, ymin = 25, ymax = 50, fill = "orange", alpha = 0.5)+
    annotate("rect", xmin = 4, xmax =27, ymin = 25, ymax = 50, fill = "orange", alpha = 0.9)+
    
    annotate("rect", xmin = 0, xmax =4, ymin = 50, ymax = 150, fill = "orange", alpha = 0.9)+
    annotate("rect", xmin = 4, xmax =27, ymin = 50, ymax = 150, fill = "red", alpha = 0.4)+
    
    annotate("rect", xmin = 0, xmax =4, ymin = 150, ymax = 1300, fill = "red", alpha = 0.4)+
    annotate("rect", xmin = 4, xmax =27, ymin =150, ymax = 1300, fill = "red", alpha = 0.5)+
    
    scale_color_viridis_d()+
    
    geom_path(show.legend = FALSE,  size =0.75, arrow = arrow(angle = 15, ends= "last", type="closed",))+
    geom_point(show.legend = FALSE, alpha = 0.8, size = 2, color = "black")+
    geom_point(show.legend = FALSE, alpha = 0.8, size = 1)+
    
    coord_cartesian(xlim = c(0, 27.5),expand = FALSE)+
    
    
    facet_wrap(~vregio)+
    
    scale_x_continuous(labels = scales::percent_format(scale = 1,accuracy = 1))+
    
    theme_classic()+
    xlab("Percentage positieve testen")+ 
    ylab("Aantal nieuwe gevallen per 100.000, per 14 dagen")+
    
    labs(title = "ECDC check",
         subtitle = "Nieuwe gevallen per 100.000, per 14 dagen  VS  percentage positieve testen \n -keurcodering volgens ECDC richtlijn-",
         caption = paste("Bron: ECDC / RIVM | Plot: @YorickB | ",Sys.Date()))+
    
    theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
           panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
           legend.position = "none",   # no legend
           plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
           plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
           
           axis.text = element_text(size=14,color = "black",face = "bold"),
           axis.text.y = element_text(face="bold", color="black", size=12),  #, angle=45),
           axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
           axis.ticks.length = unit(0.5, "cm"),
           axis.line = element_line(colour = "#F5F5F5"),
           
           panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"),
           ### facet label custom
           strip.text.x = element_text(size = 13, color = "black"),
           strip.background = element_rect(color="black", fill="gray", size=1.5, linetype="solid"))
  
  
  
  
  
  ggsave("data/63_ECDC_regio-6.png", width = 12, height = 12)  
  
  
  # transition_time(week2)+
  # labs(title = "Week: {frame_time}")+
  # shadow_wake(wake_length = 0.1, alpha = FALSE)
  
  
  ##animate(my.animation, height = 800, width =800)
  #anim_save("data/62_ECDC_ani-5.gif") 
  



 
 
 
 
