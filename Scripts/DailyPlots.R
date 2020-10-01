
library(zoo)
library(tidyverse)

# apply rolling mean over 10 data points
#Merged_data_7MA[, `:=`(rollMerged_data_7MA = frollmean(Merged_data_7MA$cases, n = 7, align = "center"), idx = .I)]
Merged_data_7MA$MACases <- rollmeanr(Merged_data_7MA$cases, 7, fill = 0)
Merged_data_7MA$MAhosp <- rollmeanr(Merged_data_7MA$hosp, 7, fill = 0)
Merged_data_7MA$MAdead <- rollmeanr(Merged_data_7MA$dead, 7, fill = 0)



Merged_data_diff_calc <- Merged_data_7MA



Merged_data_diff_calc.1 <- transform(Merged_data_diff_calc, diff_qsec = c(NA, diff(cases)))


#Merged_data_diff_calc$yesterday <-diff(Merged_data_diff_calc$cases, lag = 1, differences = 1)
#Merged_data_diff_calc(diff(as.matrix(Merged_data_diff_calc)))


#Gewenste dagen subsetten
Merged_data_short <- Merged_data_7MA[Merged_data_7MA$dateInTable>"2020-07-01"&Merged_data_7MA$dateInTable<=Sys.Date(),]
Merged_data_short$observation <- 1:nrow(Merged_data_short) 





ggplot(Merged_data_short)+
    geom_bar(stat='identity', mapping = aes(x=dateInTable, y=cases, fill = "x"))+     #, color = "#96afde"
    
    scale_fill_manual(values=c("#96afde"))+

    
    geom_line(mapping = aes(x=observation, y=MACases), color = "#44546a",lwd = 2)+

    theme_classic()+
    
    xlab("")+ 
    ylab("")+
    
    labs(title = "Nieuw gemelde besmettingen",
         subtitle = "met 7 daags voortschrijdend gemiddelde",
         caption = paste("Bron: RIVM | Plot: @YorickB ",Sys.Date()))+
    
    theme(
        plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        legend.position = "none",   # no legend
        plot.title = element_text(hjust = 0.5,size = 20,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        
        #scale_x_date(),
        
        #axis.text = element_text(size=14,color = "black",face = "bold"),
        axis.text.x=element_blank(),
        
        axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.5, "cm"),
        axis.line = element_line(colour = "#F5F5F5"),
        panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed")
    )
ggsave("data/05_new_cases.png",width=10, height = 8)




ggplot(Merged_data_short)+
    
   # ylim(32, NA)+
    
    geom_bar(stat='identity', mapping = aes(x=dateInTable, y=cases, fill = "x"))+     #, color = "#96afde"
    
    scale_fill_manual(values=c("#96afde"))+
    
    
    geom_line(mapping = aes(x=observation, y=MACases), color = "#44546a",lwd = 2)+
    
    theme_classic()+
    
    xlab("")+ 
    ylab("")+
    
    scale_y_continuous(trans='log2')+
    
    #lims(x= c(NA, NA), y = c(16, NA))+
    
    #ylim(16, NA)+
    
    labs(title = "Nieuw gemelde besmettingen, logaritmisch",
         subtitle = "met 7 daags voortschrijdend gemiddelde",
         caption = paste("Bron: RIVM | Plot: @YorickB ",Sys.Date()))+
    
    theme(
        plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        legend.position = "none",   # no legend
        plot.title = element_text(hjust = 0.5,size = 20,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        
        #scale_x_date(),
        #axis.text = element_text(size=14,color = "black",face = "bold"),
        axis.text.x=element_blank(),
        
        axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.5, "cm"),
        axis.line = element_line(colour = "#F5F5F5"),
        panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed")
    )
ggsave("data/06_new_cases_log.png",width=10, height = 8)



#par(bg="#F5F5F5") # The par() command's bg argument sets the background color for the entire plotting 
#de.bar <- barplot(Merged_data_short$cases, main="Nieuw gemelde besmettingen, logaritmisch",
##                  names.arg = Merged_data_short$dateInTable,
 #                 col = "#96afde", border = NA, log="y"
#)
#fig <- lines(x=de.bar, y = Merged_data_short$MACases, col = "#44546a",lwd = 3)
#dev.off()





ggplot(Merged_data_short)+
    geom_bar(stat='identity', mapping = aes(x=dateInTable, y=hosp, fill = "x"))+     #, color = "#96afde"
    
    scale_fill_manual(values=c("#f4b183"))+
    
    
    geom_line(mapping = aes(x=observation, y=MAhosp), color = "#c55a11",lwd = 2)+
    
    theme_classic()+
    
    xlab("")+ 
    ylab("")+
    
    labs(title = "Nieuw gemelde opnames",
         subtitle = "met 7 daags voortschrijdend gemiddelde",
         caption = paste("Bron: RIVM | Plot: @YorickB ",Sys.Date()))+
    
    theme(
        plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        legend.position = "none",   # no legend
        plot.title = element_text(hjust = 0.5,size = 20,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        
        #scale_x_date(),
        #axis.text = element_text(size=14,color = "black",face = "bold"),
        axis.text.x=element_blank(),
        
        axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.5, "cm"),
        axis.line = element_line(colour = "#F5F5F5"),
        panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed")
    )
ggsave("data/09_new_hosp.png",width=10, height = 8)


#de.bar <- barplot(Merged_data_short$hosp, main="Nieuw gemelde opnames", col = "#f4b183",border = NA)
#fig <- lines(x=de.bar, y = Merged_data_short$MAhosp, col = "#c55a11",lwd = 3)



ggplot(Merged_data_short)+
    geom_bar(stat='identity', mapping = aes(x=dateInTable, y=dead, fill = "x"))+     #, color = "#96afde"
    
    scale_fill_manual(values=c("#fab0b0"))+
    
    
    geom_line(mapping = aes(x=observation, y=MAdead), color = "#ff0505",lwd = 2)+
    
    theme_classic()+
    
    xlab("")+ 
    ylab("")+
    
    labs(title = "Nieuw gemelde overledenen",
         subtitle = "met 7 daags voortschrijdend gemiddelde",
         caption = paste("Bron: RIVM | Plot: @YorickB ",Sys.Date()))+
    
    theme(
        plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        legend.position = "none",   # no legend
        plot.title = element_text(hjust = 0.5,size = 20,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        
        #scale_x_date(),
        #axis.text = element_text(size=14,color = "black",face = "bold"),
        axis.text.x=element_blank(),
        
        axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.5, "cm"),
        axis.line = element_line(colour = "#F5F5F5"),
        panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed")
    )
ggsave("data/13_new_deceased.png",width=10, height = 8)


#de.bar <- barplot(Merged_data_short$hosp, main="Nieuw gemelde opnames", col = "#f4b183",border = NA)
#fig <- lines(x=de.bar, y = Merged_data_short$MAhosp, col = "#c55a11",lwd = 3)



#png("data/13_new_deceased.png"
#    , units = "px"
#    , height = 1600
#    , width = 1600
#    , res = 300
#)
#par(bg="#F5F5F5") # The par() command's bg argument sets the background color for the entire plotting 
#de.bar <- barplot(Merged_data_short$dead, main="Nieuw gemelde overledenen", col = "#fab0b0", border = NA)
#fig <- lines(x=de.bar, y = Merged_data_short$MAdead, col = "#ff0505",lwd = 3)
#dev.off()

