
library(tidyr)
library(dplyr)

#https://raw.githubusercontent.com/mzelst/covid-19/master/data-nice/age/leeftijdsverdeling_datum_Klinisch.csv



nice.age.ic.file <- paste0("https://raw.githubusercontent.com/mzelst/covid-19/master/data-nice/age/leeftijdsverdeling_datum_IC.csv")
nice.age.ic<-read.csv(nice.age.ic.file,sep=",",check.names=FALSE)

einde.ic = ncol(nice.age.ic)

#key <- "Leeftijd"
#value <- "leeftijd2"
#gathercols <- c("2:290")
nice.age.ic.long <- gather(nice.age.ic, key, value, 2:einde.ic)
colnames(nice.age.ic.long) = c("Leeftijd", "date", "aantal")

nice.age.ic.long$date <- as.Date(nice.age.ic.long$date)

nice.age.ic.long <- nice.age.ic.long %>% mutate(age_grouping = case_when(str_detect(Leeftijd, "<20") ~ '0-54', 
                                                                   str_detect(Leeftijd, "20 - 24") ~ '0-54',
                                                                   str_detect(Leeftijd, "25 - 29") ~ '0-54',
                                                                   str_detect(Leeftijd, "30 - 34") ~ '0-54',
                                                                   str_detect(Leeftijd, "35 - 39") ~ '0-54',
                                                                   str_detect(Leeftijd, "40 - 44") ~ '0-54',
                                                                   str_detect(Leeftijd, "45 - 49") ~ '0-54',
                                                                   str_detect(Leeftijd, "50 - 54") ~ '0-54',
                                                                   str_detect(Leeftijd, "55 - 59") ~ '55-59',
                                                                   str_detect(Leeftijd, "60 - 64") ~ '60-64',
                                                                   str_detect(Leeftijd, "65 - 69") ~ '65-69',
                                                                   str_detect(Leeftijd, "70 - 74") ~ '70-74',
                                                                   str_detect(Leeftijd, "75 - 79") ~ '75-79',
                                                                   str_detect(Leeftijd, "80 - 84") ~ '80+',
                                                                   str_detect(Leeftijd, "85 - 89") ~ '80+',
                                                                   str_detect(Leeftijd, ">90") ~ '80+'
                                                                   ))

nice.age.ic.long.count <-count(nice.age.ic.long,date,age_grouping, aantal)

#Take rolling 7-day averages
#casus.working <- casus.working %>% 
 # group_by(age_grouping) %>% 
#  arrange(date) %>% 
#  mutate(hosp_avg=roll_mean(n, 7, align="right", fill=0))

#dag<-strftime(Sys.Date()-1)

#casus.working <- casus.working[casus.working$date>"2020-02-29"&casus.working$date<dag,]
#casus.working$date <- as.Date(casus.working$date)


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



#### PLOT  onderlinge verhouding NICE IC data####

ggplot(nice.age.ic.long.count, aes(date, aantal, fill=age_grouping))+
  
  geom_bar(stat="identity", position=position_fill(), width=1) + scale_y_reverse() +
  
  theme_classic()+
  
  theme(legend.position = "right", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.background =element_rect(fill = "#E4ECFC") ,
        legend.spacing.y = unit(0, "cm"), 
        legend.key.size = unit(1, "cm"))+ 
  
  xlab("")+ 
  ylab("")+
  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%b"),
               limits = as.Date(c("2020-10-01", Sys.Date())))+
  
  scale_fill_manual(values=c("darkgray", '#f8cbad','#c55a11', '#2f5597', '#8faadc', '#5b9bd5', "black" ))+ # Use custom colors
  
  guides(fill = guide_legend(reverse = TRUE))+
  
  labs(title = "IC bezetting NICE",
     #  subtitle = "verhouding tussen de groepen, gebaseerd op 7 daags lopend gemiddelde",
       fill="",
       caption = paste("Bron data: NICE/@mzelst  | Plot: @YorickB | ",Sys.Date()-1))+
  
  theme(plot.background = element_rect(fill = "#E4ECFC"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#E4ECFC", colour = "#E4ECFC"),
        plot.title = element_text(hjust = 0.5,size = 25,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        axis.text = element_text(size=14,color = "black",face = "bold"),
        axis.ticks = element_line(colour = "#E4ECFC", size = 1, linetype = "solid"),
        axis.text.y = element_blank(),
        axis.ticks.length = unit(0.1, "cm"),
        axis.line = element_line(colour = "#E4ECFC"))+
  
  ggsave("data/03_NICE-IC-leeftijd_relatief.png",width=16, height = 9)



















nice.age.clin.file <- paste0("https://raw.githubusercontent.com/mzelst/covid-19/master/data-nice/age/leeftijdsverdeling_datum_Klinisch.csv")
nice.age.clinic<-read.csv(nice.age.clin.file,sep=",",check.names=FALSE)

einde.clin = ncol(nice.age.clinic)

#key <- "Leeftijd"
#value <- "leeftijd2"
#gathercols <- c("2:290")
nice.age.clinic.long <- gather(nice.age.clinic, key, value, 2:einde.clin)
colnames(nice.age.clinic.long) = c("Leeftijd", "date", "aantal")

nice.age.clinic.long$date <- as.Date(nice.age.clinic.long$date)

nice.age.clinic.long <- nice.age.clinic.long %>% mutate(age_grouping = case_when(str_detect(Leeftijd, "<20") ~ '0-64', 
                                                                         str_detect(Leeftijd, "20 - 24") ~ '0-64',
                                                                         str_detect(Leeftijd, "25 - 29") ~ '0-64',
                                                                         str_detect(Leeftijd, "30 - 34") ~ '0-64',
                                                                         str_detect(Leeftijd, "35 - 39") ~ '0-64',
                                                                         str_detect(Leeftijd, "40 - 44") ~ '0-64',
                                                                         str_detect(Leeftijd, "45 - 49") ~ '0-64',
                                                                         str_detect(Leeftijd, "50 - 54") ~ '0-64',
                                                                         str_detect(Leeftijd, "55 - 59") ~ '0-64',
                                                                         str_detect(Leeftijd, "60 - 64") ~ '0-64',
                                                                         str_detect(Leeftijd, "65 - 69") ~ '65-69',
                                                                         str_detect(Leeftijd, "70 - 74") ~ '70-74',
                                                                         str_detect(Leeftijd, "75 - 79") ~ '75-79',
                                                                         str_detect(Leeftijd, "80 - 84") ~ '80-84',
                                                                         str_detect(Leeftijd, "85 - 89") ~ '85-89',
                                                                         str_detect(Leeftijd, ">90") ~ '90+'
))

nice.age.clinic.count <-count(nice.age.clinic.long,date,age_grouping, aantal)

#Take rolling 7-day averages
#casus.working <- casus.working %>% 
# group_by(age_grouping) %>% 
#  arrange(date) %>% 
#  mutate(hosp_avg=roll_mean(n, 7, align="right", fill=0))

#dag<-strftime(Sys.Date()-1)

#casus.working <- casus.working[casus.working$date>"2020-02-29"&casus.working$date<dag,]
#casus.working$date <- as.Date(casus.working$date)


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



#### PLOT  onderlinge verhouding NICE IC data####

ggplot(nice.age.clinic.count, aes(date, aantal, fill=age_grouping))+
  
  geom_bar(stat="identity", position=position_fill(), width=1) + scale_y_reverse() +
  
  theme_classic()+
  
  theme(legend.position = "right", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.background =element_rect(fill = "#E4ECFC") ,
        legend.spacing.y = unit(0, "cm"), 
        legend.key.size = unit(1, "cm"))+ 
  
  xlab("")+ 
  ylab("")+
  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%b"),
               limits = as.Date(c("2020-11-03", Sys.Date())))+
  
  scale_fill_manual(values=c("darkgray", '#f8cbad','#c55a11', '#2f5597', '#8faadc', '#5b9bd5', "black" ))+ # Use custom colors
  
  guides(fill = guide_legend(reverse = TRUE))+
  
  labs(title = "Ziekenhuisbezetting NICE",
     #  subtitle = "verhouding tussen de groepen, gebaseerd op 7 daags lopend gemiddelde",
       fill="",
       caption = paste("Bron data: NICE/@mzelst  | Plot: @YorickB | ",Sys.Date()-1))+
  
  theme(plot.background = element_rect(fill = "#E4ECFC"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#E4ECFC", colour = "#E4ECFC"),
        plot.title = element_text(hjust = 0.5,size = 25,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        axis.text = element_text(size=14,color = "black",face = "bold"),
        axis.ticks = element_line(colour = "#E4ECFC", size = 1, linetype = "solid"),
         axis.text.y = element_blank(),
        axis.ticks.length = unit(0.1, "cm"),
        axis.line = element_line(colour = "#E4ECFC"))+
  
  ggsave("data/03_NICE-kliniek-leeftijd_relatief.png",width=16, height = 9)


