
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

#test.week <- yearweek(Sys.Date())

read.aantal.path <-paste("C:\\Rdir\\data\\",Sys.Date(),"\\", Sys.Date(), "_COVID-19_aantallen_gemeente_per_dag.csv", sep = "")
weeknumber.df <- read.csv(read.aantal.path,sep=";")

weeknumber.df$date <- as.Date(weeknumber.df$date)
weeknumber.df$week <- as.integer(format(weeknumber.df$date, "%Y%V"))


weeknumber.df.sh <- weeknumber.df[ -c(1:9,11:12)]

weeknumber.df.sh$date <- as.Date(weeknumber.df.sh$date)

weeknumber.df.sh$week_day <- weekdays(weeknumber.df.sh$date)

weeknumber.df.sh$week_day <- as.factor(weeknumber.df.sh$week_day)

weeknumber.df.sh.2 <- weeknumber.df.sh[weeknumber.df.sh$date > "2020-06-28",]

weeknumber.df.sh.2$weekbegin <- floor_date(weeknumber.df.sh.2$date, " week", week_start = 1)

this.week <-floor_date(as.Date(today), " week", week_start = 1)

weeknumber.df.sh.2_sum <- aggregate(weeknumber.df.sh.2$Total_reported,     by=list(dateInTable=weeknumber.df.sh.2$weekbegin ), FUN=sum)
week.level = last(weeknumber.df.sh.2_sum$x)


ggplot(weeknumber.df.sh.2, aes(x=weekbegin, y=Total_reported, fill = factor(week_day, levels=c("Sunday","Saturday","Friday","Thursday", "Wednesday", "Tuesday","Monday"))))+
  
  # geom_hline(yintercept=87500, linetype = "dashed", color = "gray")+
  
  geom_bar(stat='identity')+
  
  scale_x_date( limits = c(as.Date("2020-07-02"), NA), breaks = "2 week",  labels = date_format("%V"))+
 # coord_cartesian(expand = FALSE)+
  scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","))+
  

  #scale_fill_brewer(palette = "RdYlBu")+
  #scale_fill_manual(values = wes_palette("Darjeeling1", 7, type = "continuous"))+
  scale_fill_manual(values = wes_palette("Darjeeling1", 7, type = "continuous"))+ 
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
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
  
  labs(title = "Nieuw gemelde besmettingen per week",
       #subtitle = "met 7 daags voortschrijdend gemiddelde",
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme(legend.position = c(0.05, 0.5),
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
  
  geom_text(mapping=aes(x=as.Date("2020-07-27"), y=7875, label="Waakzaam "), size=7)+
  geom_text(mapping=aes(x=as.Date("2020-07-30"), y=4975, label="6.125 nieuwe gevallen per week"), size=4)+
  #geom_text(mapping=aes(x=30, y=67000, label="Weekrecord: week 44 - 68.488 gevallen"), size=4)+

  geom_hline(yintercept=6125,       linetype = "dashed")+
 # geom_hline(yintercept=, linetype = "dotted")
geom_segment(aes(x = as.Date("2020-09-01"), y = week.level, xend = today, yend = week.level),linetype = "dotted", color = "black")

ggsave("data/plots/65_Cases_by_week.png",width=16, height = 9)


weeknumber.df.sh.3 <- weeknumber.df.sh.2
#weeknumber.df.sh.3$week_day <- as.character(weeknumber.df.sh.3$week_day)

#weeknumber.df.sh.3$week_day <-  as.factor(weeknumber.df.sh.3$week_day, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday") )

weeknumber.df.sh.3$week_day <- factor(weeknumber.df.sh.3$week_day, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

  
 #   c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
 #   c("Sunday","Saturday","Friday","Thursday", "Wednesday", "Tuesday","Monday")


#rect2 <- data.frame(xmin = c(as.Date("2020-10-10"), as.Date("2020-10-17")),
 #                   xmax = c(as.Date("2020-10-18"), as.Date("2020-10-25")),
  #                  ymin = c(0, 0),
   #                 ymax = c(Inf, Inf),
    #                regio_vac = c("Noord", "Niet-Noord")
#          )




this.week <-floor_date(as.Date(today), " week", week_start = 1)
 


ggplot(weeknumber.df.sh.3, aes(x=weekbegin, y=Total_reported,fill = factor(week_day, levels=c("Sunday","Saturday","Friday","Thursday", "Wednesday", "Tuesday","Monday"))))+   #levels=c("Sunday","Saturday","Friday","Thursday", "Wednesday", "Tuesday","Monday"))))+
  
  geom_rect( aes(xmin = as.Date(this.week)-4,
                 xmax = as.Date(this.week)+4,
                 ymin = 0,
                ymax = Inf,
                 ), fill = "gray", alpha = 0.025)+
  
  
  geom_bar(stat='identity')+
  
  scale_x_date( limits = c(as.Date("2020-08-24"), NA), breaks = "5 week",  labels = date_format("%V"))+
 # scale_x_continuous(limits = c(as.Date("2020-08-24"), NA))+
  scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  #scale_fill_brewer(palette = "RdYlBu")+
  #scale_fill_manual(values = wes_palette("Darjeeling1", 7, type = "continuous"))+
  scale_fill_manual(values = wes_palette("Darjeeling1", 7, type = "continuous"))+ 
  
    facet_grid(~week_day)+ #, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  
  theme_bw() +
  xlab("")+ 
  ylab("")+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
   legend.position = "none",   # no legend
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
   
   axis.text = element_text(size=10,color = "black",face = "bold"),
   axis.text.y = element_text(face="bold", color="black", size=8),
   axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
   axis.ticks.length = unit(0.2, "cm"),
   
   # axis.line = element_line(colour = "black"),
   
   strip.text.x = element_text(size = 15, color = "black"),
   strip.background = element_rect(fill="gray"),  #, color="black"), #, size=0.2, linetype="solid"),
   panel.grid.major.x = element_blank(),
   panel.grid.minor.x = element_blank(),
   panel.grid.major.y = element_blank(),
   panel.grid.minor.y = element_blank(),
   )+
  
  labs(title = "Nieuw gemelde besmettingen per week",
       #subtitle = "met 7 daags voortschrijdend gemiddelde",
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))
  
ggsave("data/plots/65_Cases_by_week_facet-grid.png",width=16, height = 9)

