
#library(jsonlite)
#library(ggrepel)
#library(rtweet)
#library(tidyverse)
#library(zoo)
#library(RcppRoll)
#require(data.table)
#library(scales)


#library(viridis) 

library(wesanderson)

# week cijfers

today = Sys.Date()

current.week <- as.integer(format(Sys.Date(), "%Y%V"))


GGD.numbers.weekly <-tested_daily.count

GGD.numbers.weekly$week <- as.integer(format(GGD.numbers.weekly$date, "%Y%V"))


GGD.numbers.weekly <- GGD.numbers.weekly[ -c(2,4:11)]

GGD.numbers.weekly$week_day <- weekdays(GGD.numbers.weekly$date)

GGD.numbers.weekly$week_day <- as.factor(GGD.numbers.weekly$week_day)

GGD.numbers.weekly <- GGD.numbers.weekly[GGD.numbers.weekly$date > "2020-06-28",]

GGD.numbers.weekly$weekbegin <- floor_date(GGD.numbers.weekly$date, " week", week_start = 1)

this.week <-floor_date(as.Date(today), " week", week_start = 1)

GGD.numbers.weekly_sum <- aggregate(GGD.numbers.weekly$positive_tests,     by=list(dateInTable=GGD.numbers.weekly$weekbegin ), FUN=sum)
week.level.ggd = last(GGD.numbers.weekly_sum$x)


ggplot(GGD.numbers.weekly, aes(x=weekbegin, y=positive_tests, fill = factor(week_day, levels=c("Sunday","Saturday","Friday","Thursday", "Wednesday", "Tuesday","Monday"))))+
  

  geom_bar(stat='identity')+
  
  scale_x_date( limits = c(as.Date("2020-07-02"), NA), breaks = "2 week",  labels = date_format("%V"))+

  scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","))+
  

  scale_fill_manual(values = wes_palette("Darjeeling1", 7, type = "continuous"))+ 
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), 
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14), 
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"))+
  
  labs(title = "Nieuw gemelde besmettingen per week - door de GGD",
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme(legend.position = c(0.05, 0.5),
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
  
  geom_text(mapping=aes(x=as.Date("2020-07-27"), y=16000, label="Waakzaam "), size=7)+
  geom_text(mapping=aes(x=as.Date("2020-07-30"), y=8000, label="6.125 nieuwe gevallen per week"), size=4)+

  geom_hline(yintercept=6125,       linetype = "dashed")+
  
geom_segment(aes(x = as.Date("2020-09-01"), y = week.level.ggd, xend = today, yend = week.level.ggd),linetype = "dotted", color = "black")+

ggsave("data/plots/65_Cases_by_week_GGD.png",width=16, height = 9)





GGD.numbers.weekly$week_day <- factor(GGD.numbers.weekly$week_day, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

this.week <-floor_date(as.Date(today), " week", week_start = 1)
 


ggplot(GGD.numbers.weekly, aes(x=weekbegin, y=positive_tests,fill = factor(week_day, levels=c("Sunday","Saturday","Friday","Thursday", "Wednesday", "Tuesday","Monday"))))+   #levels=c("Sunday","Saturday","Friday","Thursday", "Wednesday", "Tuesday","Monday"))))+
  
  geom_rect( aes(xmin = as.Date(this.week)-4,
                 xmax = as.Date(this.week)+4,
                 ymin = 0,
                ymax = Inf,
                 ), fill = "gray", alpha = 0.025)+
  
  
  geom_bar(stat='identity')+
  
  scale_x_date( limits = c(as.Date("2020-08-24"), NA), breaks = "10 week",  labels = date_format("%V"))+
  scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  scale_fill_manual(values = wes_palette("Darjeeling1", 7, type = "continuous"))+ 
  
    facet_grid(~week_day)+
  
  theme_bw() +
  xlab("")+ 
  ylab("")+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), 
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
   legend.position = "none",  
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
   
   axis.text = element_text(size=10,color = "black",face = "bold"),
   axis.text.y = element_text(face="bold", color="black", size=8),
   axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
   axis.ticks.length = unit(0.2, "cm"),
   
   strip.text.x = element_text(size = 15, color = "black"),
   strip.background = element_rect(fill="gray"),
   panel.grid.major.x = element_blank(),
   panel.grid.minor.x = element_blank(),
   panel.grid.major.y = element_blank(),
   panel.grid.minor.y = element_blank(),
   )+
  
  labs(title = "Nieuw gemelde besmettingen per week - door de GGD",
   
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))
  
ggsave("data/plots/65_Cases_by_week_facet-grid_GGD.png",width=16, height = 9)

