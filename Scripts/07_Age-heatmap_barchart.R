#library(cbsodataR)
#library(sf)
#library(dplyr)
#library(extrafont)
#library(ggplot2)
library(tidyverse)
library(RcppRoll)
#require(data.table)


#bla <-cases_per_day
bla <-RIVM_casus_landelijk
#copy.aantal.landelijk.gem.dag$week<-strftime(copy.aantal.landelijk.gem.dag$date,format = "%V")

#Aantal per week per groep tellen + leeftijdverdeling landelijk pakken
bla<-count(bla,week,Agegroup)


#mergen + per honderduizen berekenen
bla   <- merge(bla,CBS_age_10yrs_GH)

bla$phd <- round(bla$n*100000/bla$population,0)


#weeknumber <- isoweek(Sys.Date())
weeknumber<-strftime(Sys.Date(),format = "%V")


#Gewenste weken subsetten
bla <- bla[bla$week>24&bla$week<=44,]


#Heatmap
ggplot(bla,aes(week,Agegroup,fill=phd))+
geom_tile(size=1.5,color="#F5F5F5")+
  geom_text(label=bla$phd,size=5)+
  scale_fill_gradient2(trans="sqrt",low = "#5B9BD5",mid="#FFEB84",midpoint = 9.5, 
                       high = "#c00000")+
  #ggtitle("Aantal geconstateerde besmettingen per 100.000 per week")+
  theme_minimal()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")+
  labs(title = "Geconstateerde besmettingen COVID-19",
       subtitle = "Aantal positief geteste mensen per 100.000 binnen de leeftijdsgroep. Week 43 & 44 zullen nog sterk stijgen",fill=NULL,
       caption = paste("Bron data: RIVM / CBS ",Sys.Date()))+
  theme(plot.background = element_rect(fill = "#F5F5F5"),
        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        plot.title = element_text(hjust = 0.5,size = 25,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        axis.text = element_text(size=14,color = "black",face = "bold"),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.3, "cm"))# +
ggsave("data/02_leeftijd_heatmap.png",width=16, height = 9)


ggplot(bla,aes(week,Agegroup,fill=phd))+
  geom_tile(size=1.5,color="#F5F5F5")+
  geom_text(label=bla$phd,size=5)+
  scale_fill_gradient2(trans="sqrt",low = "#5B9BD5",mid="#FFEB84",midpoint = 8, 
                       high = "#c00000")+
  #ggtitle("cases per 100.000 per week")+
  theme_minimal()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")+
  labs(title = "Cases COVID-19",
       subtitle = "Number of cases per 100.000, within each agegroup. Week 43 and 44 will still rise.",fill=NULL,
       caption = paste("Source: RIVM / CBS | Plot: @YorickB | ",Sys.Date()))+
  theme(plot.background = element_rect(fill = "#F5F5F5"),
        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        axis.text = element_text(size=14,color = "black",face = "bold"),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.3, "cm"))# +
ggsave("data/02_EN_leeftijd_heatmap.png",width=16, height = 9)



#Gewenste weken subsetten
bla <- bla[bla$week>35&bla$week<=43,]


#barchart
ggplot(bla,aes(Agegroup,phd,fill=week))+
  geom_bar(stat="identity", position=position_dodge(0.85),width = 0.7)+
  theme_classic()+
  
  theme(legend.position= c(0.5,0.9), legend.direction = "horizontal")+
  
  xlab("")+ 
  ylab("")+
  
  labs(title = "Geconstateerde besmettingen COVID-19",
       subtitle = "Aantal positief geteste mensen per 100.000 binnen de leeftijdsgroep. Week 43 & 44 zullen nog sterk stijgen.",
       fill="Week",
       caption = paste("Bron data: RIVM / CBS ",Sys.Date()))+
  
  theme(plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        plot.title = element_text(hjust = 0.5,size = 25, face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        axis.text = element_text(size=14,color = "black", face = "bold"),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.5, "cm"),
        axis.line = element_line(colour = "#F5F5F5"),
        panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
 scale_fill_manual(values=c('#c6cee6','#adb9dd', '#8fa2d4', '#6383c9', '#416ebd', '#3b64ad', '#f1a069', '#f8cbad' ))# +
  
ggsave("data/01_leeftijd_barchart.png",width=16, height = 9)



ggplot(bla,aes(Agegroup,phd,fill=week))+
  geom_bar(stat="identity", position=position_dodge(0.85),width = 0.7)+
  theme_classic()+
  
  theme(legend.position= c(0.5,0.9), legend.direction = "horizontal")+
  
  xlab("")+ 
  ylab("")+
  
  labs(title = "Cases COVID-19",
       subtitle = "Number of cases per 100.000, within each agegroup. Week 43 and 44 will still rise.",
       fill="Week",
       caption = paste("Source: RIVM / CBS | Plot: @YorickB | ",Sys.Date()))+
  
  theme(plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        plot.title = element_text(hjust = 0.5,size = 25, face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        axis.text = element_text(size=14,color = "black", face = "bold"),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.5, "cm"),
        axis.line = element_line(colour = "#F5F5F5"),
        panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  scale_fill_manual(values=c('#c6cee6','#adb9dd', '#8fa2d4', '#6383c9', '#416ebd', '#3b64ad', '#f1a069', '#f8cbad' ))# +

ggsave("data/01_EN_leeftijd_barchart.png",width=16, height = 9)



# Code onderlinge verhouding plot


AgeFill_t = filter(AgeFill, Agegroup != "<50" & Agegroup !="Unknown")

AgeFill_x <- AgeFill_t

AgeFill_x <- AgeFill_x %>% mutate(age_grouping = case_when(str_detect(Agegroup, "0-9") ~ '0-19', 
                                                           str_detect(Agegroup, "10-19") ~ '0-19',
                                                           str_detect(Agegroup, "20-29") ~ '20-39',
                                                           str_detect(Agegroup, "30-39") ~ '20-39',
                                                           str_detect(Agegroup, "40-49") ~ '40-59',
                                                           str_detect(Agegroup, "50-59") ~ '40-59',
                                                           str_detect(Agegroup, "60-69") ~ '60-79',
                                                           str_detect(Agegroup, "70-79") ~ '60-79',
                                                           str_detect(Agegroup, "80-89") ~ '80+',
                                                           str_detect(Agegroup, "90+") ~ '80+',))
                                             


Agefill_2 <-count(AgeFill_x,date,age_grouping)

#Agefill_Group <- AgeFill_x %>% group_by(date, age_grouping, .drop = FALSE) %>% summarise(count=sum(z)) 


#Take rolling 7-day averages
Agefill_3 <- Agefill_2 %>% 
  group_by(age_grouping) %>% 
  arrange(date) %>% 
  mutate(cases_avg=roll_mean(n, 7, align="right", fill=0))

dag<-strftime(Sys.Date()-1)

AgeFill_4 <- Agefill_3[Agefill_3$date>"2020-02-29"&Agefill_3$date<dag,]


AgeFill_4$date <- as.Date(AgeFill_4$date)



draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)
  
  grid::rectGrob(
    width = grid::unit(0.6, "npc"),
    height = grid::unit(0.6, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}

GeomBar$draw_key = draw_key_polygon3

ggplot(AgeFill_4, aes(date,cases_avg,fill=age_grouping))+
geom_bar(stat="identity", position=position_fill(), width=1) + scale_y_reverse() +
 
  theme_classic()+
  theme(legend.position = "right", legend.direction = "vertical", legend.background =element_rect(fill = "#F5F5F5") , legend.spacing.y = unit(0, "cm"), legend.key.size = unit(1, "cm"))+  #legend.spacing =0.5
  xlab("")+ 
  ylab("")+
  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%b"),
               limits = as.Date(c("2020-02-27", Sys.Date())))+
  
  guides(fill = guide_legend(reverse = TRUE))+
  
  labs(title = "Geconstateerde besmettingen COVID-19",
       subtitle = "verhouding tussen de groepen, gebaseerd op 7 daags lopend gemiddelde",
       fill="",
       caption = paste("Bron data: RIVM ",Sys.Date()-1))+
  
  
  
  #scale_x_discrete(breaks = AgeFill_4$date[seq(1, length(AgeFill_4$date), by =  330)])+            #155)])+
    
  
   theme(plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        plot.title = element_text(hjust = 0.5,size = 25,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
       
        axis.text = element_text(size=14,color = "black",face = "bold"),
        
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
        #axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.length = unit(0.1, "cm"),
        axis.line = element_line(colour = "#F5F5F5"))+ 
      #  panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed")
    scale_fill_manual(values=c('#f8cbad','#c55a11', '#2f5597', '#8faadc', '#5b9bd5')) # Use custom colors

ggsave("data/03_leeftijd_relatief.png",width=16, height = 9)


ggplot(AgeFill_4, aes(date,cases_avg,fill=age_grouping))+
  geom_bar(stat="identity", position=position_fill(), width=1) + scale_y_reverse() +
  
  theme_classic()+
  theme(legend.position = "right", legend.direction = "vertical", legend.background =element_rect(fill = "#F5F5F5") , legend.spacing.y = unit(0, "cm"), legend.key.size = unit(1, "cm"))+  #legend.spacing =0.5
  xlab("")+ 
  ylab("")+
  guides(fill = guide_legend(reverse = TRUE))+
  
  labs(title = "Cases COVID-19",
       subtitle = "relationship between the age groups, based on the 7 day moving average",fill=NULL,
       caption = paste("Source: RIVM / CBS | Plot: @YorickB | ",Sys.Date()))+
  #scale_x_discrete(breaks = AgeFill_4$date[seq(1, length(AgeFill_4$date), by =  330)])+            #155)])+
  theme(plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        plot.title = element_text(hjust = 0.5,size = 25,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        axis.text = element_text(size=14,color = "black",face = "bold"),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.length = unit(0.1, "cm"),
        axis.line = element_line(colour = "#F5F5F5"))+ 
  #  panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed")
  scale_fill_manual(values=c('#f8cbad','#c55a11', '#2f5597', '#8faadc', '#5b9bd5')) # Use custom colors

ggsave("data/03_EN_leeftijd_relatief.png",width=16, height = 9)



#scale_fill_brewer(palette="Blues")+
#scale_fill_manual(values=c('#c6cee6','#adb9dd', '#8fa2d4', '#6383c9', '#416ebd', '#3b64ad', '#f4b183', '#f8cbad' )) # Use custom colors
# scale_fill_manual(values=c('#c6cee6','#adb9dd', '#8fa2d4', '#6383c9', '#416ebd', '#3b64ad', '#f1a069', '#f8cbad' ))
