library(tidyverse)
library(gganimate)
library(transformr)

library(jsonlite)
library(lubridate)

library(zoo)
require(data.table)


dat <- fromJSON(txt = "https://coronadashboard.rijksoverheid.nl/json/NL.json")
tested_daily <- as.data.frame(dat$tested_ggd[1])
tested_daily$date <- as.Date(as.POSIXct(tested_daily$values.date_unix, origin="1970-01-01"))


tested_daily.toedit <-tested_daily
tested_daily.toedit <- tested_daily.toedit[ -c(1,3,5:8)]

colnames(tested_daily.toedit) = c("cases", "perc_pos", "date")

tested_daily.toedit <- tested_daily.toedit %>%
  mutate(date = as.Date(date, "%d-%m-%Y")) %>%
  arrange(date)

tested_daily.toedit$forteend <- (rollsum(tested_daily.toedit$cases, 14, fill = 0, align = "right"))
tested_daily.toedit$vdphd <- tested_daily.toedit$forteend/17474677*100000



tested_daily.toedit <- (tested_daily.toedit  %>% filter(date > "2020-06-14"))


#####

last.point <- last(tested_daily.toedit)


ggplot(tested_daily.toedit, aes(perc_pos, vdphd, colour=date))+
  
  annotate("rect", xmin = 0, xmax =1, ymin = 500, ymax = 2000, color = "black",fill = "darkred", alpha = 0.9)+
  annotate("rect", xmin = 1, xmax =27, ymin =500, ymax = 2000, color = "black",fill = "darkred", alpha = 0.9)+
  annotate("rect", xmin = 4, xmax =27, ymin =500, ymax = 2000, color = "black",fill = "darkred", alpha = 0.9)+
  
  annotate("rect", xmin = 0, xmax =1, ymin = 200, ymax = 500, color = "black",fill = "red", alpha = 0.5)+
  annotate("rect", xmin = 1, xmax =4, ymin = 200, ymax = 500, color = "black",fill = "red", alpha = 0.5)+
  annotate("rect", xmin = 4, xmax =27, ymin =200, ymax = 500, color = "black",fill = "red", alpha = 0.5)+
  
  annotate("rect", xmin = 0, xmax =1, ymin = 75, ymax = 200, color = "black",fill = "orange", alpha = 0.9)+
  annotate("rect", xmin = 1, xmax =4, ymin = 75, ymax = 200, color = "black",fill = "orange", alpha = 0.9)+
  annotate("rect", xmin = 4, xmax =27, ymin = 75, ymax = 200, color = "black",fill = "red", alpha = 0.5)+
  
  annotate("rect", xmin = 0, xmax =1, ymin = 50, ymax = 75, color = "black", fill = "green", alpha = 0.9)+
  annotate("rect", xmin = 1, xmax =4, ymin = 50, ymax = 75, color = "black", fill = "orange", alpha = 0.9)+
  annotate("rect", xmin = 4, xmax =27, ymin = 50, ymax = 75,color = "black", fill = "orange", alpha = 0.9)+ 
  
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 50,color = "black", fill = "green", alpha = 0.75)+
  annotate("rect", xmin = 1, xmax = 4, ymin = 0, ymax = 50,color = "black", fill = "green", alpha = 0.75)+
  annotate("rect", xmin = 4, xmax = 27, ymin = 0, ymax = 50,color = "black", fill = "orange", alpha = 0.9)+ 
  
  
  
  
  
  geom_path(   size =0.5, arrow = arrow(angle = 20,type = "open"), color = "black")+
  geom_point( color = "black", size =4)+
  geom_point(   size =3)+
  
  geom_point(data=last.point, color="green", size = 3)+
  
  scale_x_continuous(labels = scales::percent_format(scale = 1,accuracy = 1), breaks = c(0,1,4,5,10,15,20,25,30))+ #, trans = log2_trans())+
  scale_color_gradient(low="gray", high="darkblue")+
  
  scale_y_continuous(limits = c(0, 2000),breaks = c(0,50,75,200,500,1000,2000))+
  
  theme_classic()+
  xlab("Percentage positieve testen")+ 
  ylab("Aantal nieuwe gevallen per 100.000, per 14 dagen")+
  
  labs(title = "ECDC check - GGD data",
       subtitle = "Nieuwe gevallen per 100.000, per 14 dagen  VS  percentage positieve testen \n -keurcodering volgens ECDC richtlijn-",
       caption = paste("vanaf 2020-06-14  -- Bron: ECDC / RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/2esize (border color and size)
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



ggsave("data/plots/63_ECDC_6.png", width = 16, height = 9)  
