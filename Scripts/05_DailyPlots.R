
library(zoo)
library(tidyverse)


Merged_data_7MA <- Merged_data_2


#Gewenste dagen subsetten
Merged_data_short <- Merged_data_7MA[Merged_data_7MA$dateInTable>"2020-07-01"&Merged_data_7MA$dateInTable<=Sys.Date(),]
Merged_data_short$observation <- 1:nrow(Merged_data_short) 


Merged_data_short$fixedDate <- as.Date(Merged_data_short$dateInTable,format="%Y-%m-%d")


test.date.df =data.frame(date=as.Date(c("2020-12-01")),event="verruiming testbeleid")



persco.df=data.frame(date=as.Date(c("2020-10-14",   "2020-12-15", "2020-12-25", "2021-01-01", "2021-01-12","2021-01-19")), 
                     event=c("Semi-lockdown",   "lockdown", "kerst", "1 jan", "persco", "einde lockdown?"))

persco.df.2=data.frame(date=as.Date(c("2020-12-25", "2021-01-01", "2021-01-12","2021-01-19")), 
                     event=c( "kerst", "1 jan", "persco", "einde lockdown?"))


new.cases.title.number  <- format( (last(Merged_data_7MA$cases)), big.mark="." ,decimal.mark=",")
new.cases.title <-  paste("Nieuw gemelde besmettingen  -  ", new.cases.title.number )


ggplot(Merged_data_short)+
    geom_bar(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"))+     #, color = "#96afde"
    
    #geom_vline(xintercept = as.numeric(Merged_data_short$fixedDate[dates_vline_vak2]),
     #          col = "darkgray", lwd = 1, linetype= "dashed")+
        scale_fill_manual(values=c("#96afde"))+
    geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#F5F5F5",lwd = 3)+
    geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#44546a",lwd = 2)+
    #geom_line(mapping = aes(x=fixedDate, y=ma_c_lag), color = "red",lwd = 2)+
    
  #  scale_x_date(date_breaks = "1 month", 
  #               date_labels= format("%b"),
  #               limits = as.Date(c("2020-09-01", "2021-02-05")))+
    
    scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","))+
    
    #geom_vline(data=test.date.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "black")+
    #geom_text(data=test.date.df  , mapping=aes(x=date, y=11000, label=event), size=7, angle=-90, vjust=-0.4, hjust=0, color= "black")+
    
    #geom_vline(data=persco.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
   # geom_text(data=persco.df  , mapping=aes(x=date, y=3000, label=event), size=5, angle=-90, vjust=-0.4, hjust=0, color= "black")+
    
    #geom_vline(data=persco.df.2,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "darkgray")+
   # geom_text(data=persco.df.2  , mapping=aes(x=date, y=3000, label=event), size=5, angle=-90, vjust=-0.4, hjust=0, color= "darkgray")+
    
    
    theme_classic()+
    xlab("")+ 
    ylab("")+
    labs(title = new.cases.title,
         subtitle = "met 7 daags voortschrijdend gemiddelde",
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
ggsave("data/plots/05_new_cases.png",width=16, height = 9)



ggplot(Merged_data_short)+
    geom_bar(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"))+     #, color = "#96afde"
    scale_fill_manual(values=c("#96afde"))+
    geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#F5F5F5",lwd = 3)+
    geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#44546a",lwd = 2)+
    theme_classic()+
    xlab("")+ 
    ylab("")+
    scale_y_continuous( limits = c(0, 7000), labels = label_comma(big.mark = ".", decimal.mark = ","))+
  scale_x_date(date_breaks = "3 day", 
               date_labels= format("%d %b"),
               limits = as.Date(c("2021-06-25", NA)))+ 
   labs(title = "New cases",
         subtitle = "with 7 day moving average",
         caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+
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
#ggsave("data/05_EN_new_cases.png",width=16, height = 9)




ggplot(Merged_data_short)+
    geom_bar(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"))+     #, color = "#96afde"
    scale_fill_manual(values=c("#96afde"))+
    geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#F5F5F5",lwd = 3)+
    geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#44546a",lwd = 2)+
    theme_classic()+
    xlab("")+ 
    ylab("")+
    scale_y_continuous(limits = c(1024, 33000), trans='log2', labels = label_comma(big.mark = ".", decimal.mark = ","))+
    labs(title = "Nieuw gemelde besmettingen, logaritmisch",
         subtitle = "met 7 daags voortschrijdend gemiddelde",
         caption = paste("Bron: RIVM | Plot: @YorickB  | ",Sys.Date()))+
    theme(
        plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        legend.position = "none",   # no legend
        plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        #scale_x_date(),
        axis.text = element_text(size=14,color = "black",face = "bold"),
       # axis.text.x=element_blank(),
        axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.5, "cm"),
        axis.line = element_line(colour = "#F5F5F5"),
        panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))
ggsave("data/06_new_cases_log_line.png",width=16, height = 9)



ggplot(Merged_data_short)+
    geom_bar(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"))+     #, color = "#96afde"
    scale_fill_manual(values=c("#96afde"))+
    geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#F5F5F5",lwd = 3)+
    geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#44546a",lwd = 2)+
    theme_classic()+
    xlab("")+ 
    ylab("")+
    scale_y_continuous(trans='log2',labels = label_comma(big.mark = ".", decimal.mark = ","))+
    #lims(x= c(NA, NA), y = c(16, NA))+
    #ylim(16, NA)+
    labs(title = "New cases, logaritmic scale",
         subtitle = "with 7 day moving average",
         caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+
    theme(
        plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        legend.position = "none",   # no legend
        plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        #scale_x_date(),
        axis.text = element_text(size=14,color = "black",face = "bold"),
        # axis.text.x=element_blank(),
        axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.5, "cm"),
        axis.line = element_line(colour = "#F5F5F5"),
        panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))
#ggsave("data/06_EN_new_cases_log.png",width=16, height = 9)




ggplot(Merged_data_short)+
    geom_bar(stat='identity', mapping = aes(x=fixedDate, y=hosp, fill = "x"))+     #, color = "#96afde"
    scale_fill_manual(values=c("#f4b183"))+
    geom_line(mapping = aes(x=fixedDate, y=ma_h_lead), color = "#F5F5F5",lwd = 3)+
    geom_line(mapping = aes(x=fixedDate, y=ma_h_lead), color = "#c55a11",lwd = 2)+
    theme_classic()+
    xlab("")+ 
    ylab("")+
    labs(title = "Nieuw gemelde opnames",
         subtitle = "met 7 daags voortschrijdend gemiddelde",
         caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
    theme(
        plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        legend.position = "none",   # no legend
        plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        #scale_x_date(),
       axis.text = element_text(size=14,color = "black",face = "bold"),
       # axis.text.x=element_blank(),
        axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.5, "cm"),
        axis.line = element_line(colour = "#F5F5F5"),
        panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))
ggsave("data/09_new_hosp.png",width=16, height = 9)




ggplot(Merged_data_short)+
    geom_bar(stat='identity', mapping = aes(x=fixedDate, y=hosp, fill = "x"))+     #, color = "#96afde"
    scale_fill_manual(values=c("#f4b183"))+
    geom_line(mapping = aes(x=fixedDate, y=ma_h_lead), color = "#F5F5F5",lwd = 3)+
    geom_line(mapping = aes(x=fixedDate, y=ma_h_lead), color = "#c55a11",lwd = 2)+
    theme_classic()+
    xlab("")+ 
    ylab("")+
    labs(title = "New hospitalizations",
         subtitle = "with 7 day moving average",
         caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+
    theme(
        plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        legend.position = "none",   # no legend
        plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        #scale_x_date(),
        axis.text = element_text(size=14,color = "black",face = "bold"),
        # axis.text.x=element_blank(),
        axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.5, "cm"),
        axis.line = element_line(colour = "#F5F5F5"),
        panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))
#ggsave("data/09_EN_new_hosp.png",width=16, height = 9)




ggplot(Merged_data_short)+
    geom_bar(stat='identity', mapping = aes(x=fixedDate, y=dead, fill = "x"))+     #, color = "#96afde"
    scale_fill_manual(values=c("#fab0b0"))+
    geom_line(mapping = aes(x=fixedDate, y=ma_d_lead), color = "#F5F5F5",lwd = 3)+
    geom_line(mapping = aes(x=fixedDate, y=ma_d_lead), color = "#ff0505",lwd = 2)+
    theme_classic()+
    xlab("")+ 
    ylab("")+
    labs(title = "Nieuw gemelde overledenen",
         subtitle = "met 7 daags voortschrijdend gemiddelde",
         caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
    theme(
        plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        legend.position = "none",   # no legend
        plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        #scale_x_date(),
       axis.text = element_text(size=14,color = "black",face = "bold"),
       # axis.text.x=element_blank(),
        axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.5, "cm"),
        axis.line = element_line(colour = "#F5F5F5"),
        panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))
ggsave("data/13_new_deceased.png",width=16, height = 9)


ggplot(Merged_data_short)+
    geom_bar(stat='identity', mapping = aes(x=fixedDate, y=dead, fill = "x"))+     #, color = "#96afde"
    scale_fill_manual(values=c("#fab0b0"))+
    geom_line(mapping = aes(x=fixedDate, y=ma_d_lead), color = "#F5F5F5",lwd = 3)+
    geom_line(mapping = aes(x=fixedDate, y=ma_d_lead), color = "#ff0505",lwd = 2)+
    theme_classic()+
    xlab("")+ 
    ylab("")+
    labs(title = "New deceased",
         subtitle = "with 7 day moving average",
         caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+
    theme(
        plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        legend.position = "none",   # no legend
        plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        #scale_x_date(),
        axis.text = element_text(size=14,color = "black",face = "bold"),
        # axis.text.x=element_blank(),
        axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.5, "cm"),
        axis.line = element_line(colour = "#F5F5F5"),
        panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))
#ggsave("data/13_EN_new_deceased.png",width=16, height = 9)



daily.numbers.filename <- paste0("data/plots/",format(Sys.Date(), "%Y-%m-%d"), "_daily_numbers.csv")
write.csv2(Merged_data_7MA, file = daily.numbers.filename, row.names = F)


#### doubling check cases ####
Merged_data_short_2021 <- Merged_data_short[Merged_data_short$fixedDate>"2021-06-01",]


minMA_summer2021 <- min(Merged_data_short_2021$MACases)
current_MA <- last(Merged_data_short_2021$MACases)

#minMA_summer2021 <- 600
#current_MA <- 1100

increase1a <- (current_MA/minMA_summer2021)*100
increase1a <- round(increase1a, digits =0)


doubling1a <- log2(current_MA/minMA_summer2021)
doubling1a <- round(doubling1a, digits =2)


new.cases.subtitle <- paste0("Stijging van het 7 daags voortschrijdend gemiddelde tov het dal: ", increase1a , "%\n",
                             "Dat is: ", doubling1a, " verdubbeling")

ggplot(Merged_data_short_2021)+

    geom_bar(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"))+
  
  scale_fill_manual(values=c("#96afde"))+
  geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#F5F5F5",lwd = 3)+
  geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#44546a",lwd = 2)+
  
  scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
theme_classic()+
  xlab("")+ 
  ylab("")+
  labs(title = new.cases.title,
       subtitle =  "met 7 daags voortschrijdend gemiddelde",
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), 
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    legend.position = "none", 
    plot.title    =  element_text(hjust = 0.5 , size = 30, face = "bold"),
    plot.subtitle =  element_text(hjust = 0.5 , size = 15, color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14), 
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))
ggsave("data/plots/05_new_cases_2021.png",width=16, height = 9)



