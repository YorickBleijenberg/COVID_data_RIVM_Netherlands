

##### hospitalizations


casus.working = filter(RIVM_casus_landelijk, Agegroup != "<50" & Agegroup !="Unknown")

casus.working = filter(RIVM_casus_landelijk, Hospital_admission == "Yes")

casus.working <- casus.working %>% mutate(age_grouping = case_when(str_detect(Agegroup, "0-9") ~ '0-39', 
                                                                   str_detect(Agegroup, "10-19") ~ '0-39',
                                                                   str_detect(Agegroup, "20-29") ~ '0-39',
                                                                   str_detect(Agegroup, "30-39") ~ '0-39',
                                                                   str_detect(Agegroup, "40-49") ~ '40-49',
                                                                   str_detect(Agegroup, "50-59") ~ '50-59',
                                                                   str_detect(Agegroup, "60-69") ~ '60-69',
                                                                   str_detect(Agegroup, "70-79") ~ '70-79',
                                                                   str_detect(Agegroup, "80-89") ~ '80-89',
                                                                   str_detect(Agegroup, "90+") ~ '90+',))

casus.working <-count(casus.working,date,age_grouping)

#Take rolling 7-day averages
casus.working <- casus.working %>% 
  group_by(age_grouping) %>% 
  arrange(date) %>% 
  mutate(hosp_avg=roll_mean(n, 7, align="right", fill=0))

dag<-strftime(Sys.Date()-1)

casus.working <- casus.working[casus.working$date>"2020-02-29"&casus.working$date<dag,]
casus.working$date <- as.Date(casus.working$date)


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


#### PLOT  onderlinge verhouding ####

ggplot(casus.working, aes(date, hosp_avg, fill=age_grouping))+
  
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

  labs(title = "Ziekenhuisopnames COVID-19",
       subtitle = "verhouding tussen de groepen, gebaseerd op 7 daags lopend gemiddelde",
       fill="",
       caption = paste("Bron data: RIVM  | Plot: @YorickB | ",Sys.Date()-1))+
  
  theme(plot.background = element_rect(fill = "#E4ECFC"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#E4ECFC", colour = "#E4ECFC"),
        plot.title = element_text(hjust = 0.5,size = 25,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        axis.text = element_text(size=14,color = "black",face = "bold"),
        axis.ticks = element_line(colour = "#E4ECFC", size = 1, linetype = "solid"),
        axis.text.y = element_blank(),
        axis.ticks.length = unit(0.1, "cm"),
        axis.line = element_line(colour = "#E4ECFC"))+

  ggsave("data/03_leeftijd_relatief_hosp.png",width=16, height = 9)










##### Deaths###


casus.working = filter(RIVM_casus_landelijk, Agegroup !="Unknown")

casus.working = filter(RIVM_casus_landelijk, Deceased == "Yes")

casus.working <- casus.working %>% mutate(age_grouping = case_when(str_detect(Agegroup, "<50") ~ '0-49', 
                                                                   str_detect(Agegroup, "50-59") ~ '50-59',
                                                                   str_detect(Agegroup, "60-69") ~ '60-69',
                                                                   str_detect(Agegroup, "70-79") ~ '70-79',
                                                                   str_detect(Agegroup, "80-89") ~ '80-89',
                                                                   str_detect(Agegroup, "90+") ~ '90+',))

casus.working <-count(casus.working,date,age_grouping)

#Take rolling 7-day averages
#casus.working <- casus.working %>% 
#  group_by(age_grouping) %>% 
#  arrange(date) %>% 
#  mutate(cases_avg=roll_mean(n, 7, align="right", fill=0))

dag<-strftime(Sys.Date()-1)

casus.working <- casus.working[casus.working$date>"2020-02-29"&casus.working$date<dag,]
casus.working$date <- as.Date(casus.working$date)


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


#### PLOT  onderlinge verhouding ####

ggplot(casus.working, aes(date, n, fill=age_grouping))+
  
  geom_bar(stat="identity", position=position_fill(), width=1) + scale_y_reverse() +
  
  theme_classic()+
  
  theme(legend.position = "right", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.background =element_rect(fill = "#FDE3E3") ,
        legend.spacing.y = unit(0, "cm"), 
        legend.key.size = unit(1, "cm"))+ 
  
  xlab("")+ 
  ylab("")+
  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%b"),
               limits = as.Date(c("2020-10-01", Sys.Date())))+
  
  # scale_y_continuous( label = percent_format(), sec.axis = sec_axis(~ . * 1, label = percent_format()))+
  
  
  scale_fill_manual(values=c("darkgray", '#c55a11', '#2f5597', '#8faadc', '#5b9bd5', "black"))+ # Use custom colors
  
  guides(fill = guide_legend(reverse = TRUE))+
  
  labs(title = "Overleden COVID-19",
      # subtitle = "verhouding tussen de groepen, gebaseerd op 7 daags lopend gemiddelde",
       fill="",
       caption = paste("Bron data: RIVM  | Plot: @YorickB | ",Sys.Date()-1))+
  
  theme(plot.background = element_rect(fill = "#FDE3E3"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#FDE3E3", colour = "#FDE3E3"),
        plot.title = element_text(hjust = 0.5,size = 25,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        axis.text = element_text(size=14,color = "black",face = "bold"),
        axis.ticks = element_line(colour = "#FDE3E3", size = 1, linetype = "solid"),
        axis.text.y = element_blank(),
        axis.ticks.length = unit(0.1, "cm"),
        axis.line = element_line(colour = "#FDE3E3"))+
  
  ggsave("data/03_leeftijd_relatief_death.png",width=16, height = 9)

