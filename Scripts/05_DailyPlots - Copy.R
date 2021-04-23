
library(zoo)
library(tidyverse)


Merged_data_7MA <- Merged_data_2

cumulatief <- RIVM_aantallen_gemeente_cumulatief



cumulatief.count <- aggregate(cumulatief$Total_reported,     by=list(dateInTable=cumulatief$date), FUN=sum)
cumulatief.count.dead <- aggregate(cumulatief$Deceased,     by=list(dateInTable=cumulatief$date), FUN=sum)


log.log <- merge(cumulatief.count, cumulatief.count.dead, by = "dateInTable", all.x = TRUE)


log.log <- merge(log.log, Merged_data_2, by = "dateInTable", all.x = TRUE)


last.log.log <- last(log.log)


#Gewenste dagen subsetten
Merged_data_short <- Merged_data_7MA[Merged_data_7MA$dateInTable>"2020-07-01"&Merged_data_7MA$dateInTable<=Sys.Date(),]
Merged_data_short$observation <- 1:nrow(Merged_data_short) 


Merged_data_short$fixedDate <- as.Date(Merged_data_short$dateInTable,format="%Y-%m-%d")


test.date.df =data.frame(date=as.Date(c("2020-12-01")),event="verruiming testbeleid")



persco.df=data.frame(date=as.Date(c("2020-10-14",   "2020-12-15", "2020-12-25", "2021-01-01", "2021-01-12","2021-01-19")), 
                     event=c("Semi-lockdown",   "lockdown", "kerst", "1 jan", "persco", "einde lockdown?"))

persco.df.2=data.frame(date=as.Date(c("2020-12-25", "2021-01-01", "2021-01-12","2021-01-19")), 
                     event=c( "kerst", "1 jan", "persco", "einde lockdown?"))



new.cases.title <-  paste("Nieuw gemelde besmettingen  -  ",  (last(Merged_data_7MA$cases)))


ggplot(log.log)+
    
  geom_point(aes(x=x.x, y= MACases))+
  
  geom_point(aes(x=x.y, y= MAdead), color = "red")+
  
  geom_point(data=last.log.log, aes(x=x.x, y= MACases), color = "green")+
  geom_point(data=last.log.log, aes(x=x.y, y= MAdead), color = "green")+
  
  scale_x_continuous(trans = 'log2', breaks = c(10,100,1000,10000, 100000, 1000000),labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(trans = 'log2', breaks = c(10,100,1000,10000),labels = label_comma(big.mark = ".", decimal.mark = ",")) +
    
    theme_classic()+
    xlab("")+ 
    ylab("")+
    labs(title = "Nieuwe gevallen cumulatief vs. Nieuwe gevallen",
         subtitle = "In rood: doden. \nlog, log",
         caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
    theme(
        plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        legend.position = "none",   # no legend
        plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
       #scale_x_date(),
        axis.text = element_text(size=14,color = "black",face = "bold"),
       #axis.text.x=element_blank(),
        axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.5, "cm"),
        axis.line = element_line(colour = "#F5F5F5"),
        panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
ggsave("data/plots/05_log_log2.png",width=16, height = 9)


###
###






log.ani <- ggplot(log.log)+
  
  geom_point(aes(x=x.x, y= MACases))+
  
   geom_point(data=last.log.log, aes(x=x.x, y= MACases), color = "green")+

  
  scale_x_continuous(trans = 'log2', breaks = c(10,100,1000,10000, 100000, 1000000),labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(trans = 'log2', breaks = c(10,100,1000,10000),labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  labs(title = "Nieuwe gevallen cumulatief vs. Nieuwe gevallen",
       subtitle = "In rood: doden. \nlog, log",
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    legend.position = "none",   # no legend
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    #scale_x_date(),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    #axis.text.x=element_blank(),
    axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))


transition_time(x.x) +
  labs(title = "Datum: {frame_time}") +
  view_follow(fixed_y = TRUE)+
  shadow_wake(wake_length = 0.0, alpha = FALSE)

animate(log.ani, fps=4)


anim_save("data/60_log-log.gif",width=16, height = 9)  
