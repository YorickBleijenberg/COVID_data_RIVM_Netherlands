library(tidyverse)
#require(dplyr)
library(zoo)


vacc.effect.age.file <- paste0("https://raw.githubusercontent.com/mzelst/covid-19/master/data-nice/age/leeftijdsverdeling_datum_Klinisch_IC_long.csv")
vacc.effect.age <-read.csv(vacc.effect.age.file,sep=",", check.names = FALSE)
vacc.effect.age$Datum <- as.Date(vacc.effect.age$Datum)

#### select type

type.of.data = "Klinisch"


vacc.effect.age.ICU <- (vacc.effect.age  %>% filter(Type == type.of.data & Datum > "2020-09-15"))  #  Klinisch


vacc.effect.age.ICU$young = vacc.effect.age.ICU$`20 - 24`+vacc.effect.age.ICU$`25 - 29`+
  vacc.effect.age.ICU$`30 - 34`+vacc.effect.age.ICU$`35 - 39`+vacc.effect.age.ICU$`40 - 44`+
  vacc.effect.age.ICU$`45 - 49`   #  +vacc.effect.age.ICU$`<20`
vacc.effect.age.ICU$middle = vacc.effect.age.ICU$`50 - 54` + vacc.effect.age.ICU$`55 - 59` +
                            vacc.effect.age.ICU$`60 - 64` + vacc.effect.age.ICU$`65 - 69`
vacc.effect.age.ICU$old  =  vacc.effect.age.ICU$`70 - 74` + vacc.effect.age.ICU$`75 - 79`
vacc.effect.age.ICU$old80  =  vacc.effect.age.ICU$`80 - 84` 
vacc.effect.age.ICU$old85p  =  vacc.effect.age.ICU$`85 - 89` +vacc.effect.age.ICU$`>90`

#vacc.effect.age.ICU$old85  =  vacc.effect.age.ICU$`85 - 89`
#vacc.effect.age.ICU$old90  =  vacc.effect.age.ICU$`>90`

#vacc.effect.age.ICU <- vacc.effect.age.ICU[ -c(2)]

vacc.effect.age.ICU <- (vacc.effect.age.ICU %>%  mutate(young.Change  = young  - lag(young, default = young[1])) )
vacc.effect.age.ICU <- (vacc.effect.age.ICU %>%  mutate(middle.Change = middle - lag(middle, default = middle[1])) )
vacc.effect.age.ICU <- (vacc.effect.age.ICU %>%  mutate(old.Change    = old    - lag(old, default = old[1])) )
vacc.effect.age.ICU <- (vacc.effect.age.ICU %>%  mutate(old80.Change  = old80  - lag(old80, default = old80[1])) )
vacc.effect.age.ICU <- (vacc.effect.age.ICU %>%  mutate(old85p.Change  = old85p  - lag(old85p, default = old85p[1])) )
#vacc.effect.age.ICU <- (vacc.effect.age.ICU %>%  mutate(old90.Change  = old90  - lag(old90, default = old90[1])) )


vacc.effect.age.ICU$MA.young.Change   <- rollmeanr(vacc.effect.age.ICU$young.Change  , 7, fill = 0)
vacc.effect.age.ICU$MA.middle.Change  <- rollmeanr(vacc.effect.age.ICU$middle.Change , 7, fill = 0)
vacc.effect.age.ICU$MA.old.Change     <- rollmeanr(vacc.effect.age.ICU$old.Change    , 7, fill = 0)
vacc.effect.age.ICU$MA.old80.Change   <- rollmeanr(vacc.effect.age.ICU$old80.Change  , 7, fill = 0)
vacc.effect.age.ICU$MA.old85p.Change   <- rollmeanr(vacc.effect.age.ICU$old85p.Change  , 7, fill = 0)
#vacc.effect.age.ICU$MA.old90.Change   <- rollmeanr(vacc.effect.age.ICU$old90.Change  , 7, fill = 0)


maxClinyoung <- max(vacc.effect.age.ICU$MA.young.Change, na.rm = TRUE)
maxClinMid   <- max(vacc.effect.age.ICU$MA.middle.Change, na.rm = TRUE)
maxClinOld   <- max(vacc.effect.age.ICU$MA.old.Change, na.rm = TRUE)
maxClin80    <- max(vacc.effect.age.ICU$MA.old80.Change, na.rm = TRUE)
maxClin85p    <- max(vacc.effect.age.ICU$MA.old85p.Change, na.rm = TRUE)
#maxClin90    <- max(vacc.effect.age.ICU$MA.old90.Change, na.rm = TRUE)


vacc.effect.age.ICU$MA.young.Change.relative  <- vacc.effect.age.ICU$MA.young.Change /maxClinyoung
vacc.effect.age.ICU$MA.middle.Change.relative <- vacc.effect.age.ICU$MA.middle.Change/maxClinMid
vacc.effect.age.ICU$MA.old.Change.relative    <- vacc.effect.age.ICU$MA.old.Change   /maxClinOld
vacc.effect.age.ICU$MA.old80.Change.relative  <- vacc.effect.age.ICU$MA.old80.Change /maxClin80
vacc.effect.age.ICU$MA.old85p.Change.relative  <- vacc.effect.age.ICU$MA.old85p.Change /maxClin85p
#vacc.effect.age.ICU$MA.old90.Change.relative  <- vacc.effect.age.ICU$MA.old90.Change /maxClin90


relative.table.clinic <- vacc.effect.age.ICU[ -c(2:33)]
relative.table.clinic <- relative.table.clinic[relative.table.clinic$Datum >"2020-10-15",]


key <- "Datum"
value <- "test"
relative.table.clinic.long <- gather(relative.table.clinic, key, value, 2:6)

relative.table.clinic.long$key <- as.factor(relative.table.clinic.long$key)


#######   

ggplot(relative.table.clinic.long, aes(x=Datum, y= value, color = key))+
  geom_line( size=2 )+
  
#  scale_y_continuous(labels = percent)+
  scale_x_date(date_breaks = "1 months",date_labels= format("%b"))+
  # limits = as.Date(c("2021-01-6", NA)))+
  
  #coord_cartesian(expand = FALSE)+
  theme_classic()+
  xlab("")+
  ylab("")+
  
  #scale_fill_manual( values=c("#5c146e", "#fca50a", "darkgreen", "#dd513a"), labels=c("zorginstellingen", "GGD'en","Huisartsen", "ziekenhuizen" ))+
  #scale_color_brewer(palette = "RdYlBu", labels=c("zorginstellingen", "GGD'en","Huisartsen", "ziekenhuizen" ))+

  scale_color_brewer(palette = "RdYlBu", labels=c("50-69", "70-79","80-84", "85+", "50-" ))+  
 
  # scale_color_manual( values=c("#1F968BFF", "#481567FF"), labels=c("besmettingen totaal", "besmettingen verpleeghuizen" ))+
  
  labs(title = "Opnames Kliniek naar leeftijd",
       #subtitle = "Blauw = Nieuwe besmettingen verpleeghuis \n Rood = Dagelijkse cijfers besmettingen",
       caption = paste("Bron: NICE | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(legend.position = "right",  #c(0.5, 0.925),
        legend.background = element_rect(fill="#DAE3F3",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=27, face="bold"))+
  
  theme(
    plot.background = element_rect(fill = "#DAE3F3"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#DAE3F3", colour = "#DAE3F3"),
    plot.title = element_text(hjust = 0.5,size = 40,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.ticks = element_line(colour = "#DAE3F3", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#DAE3F3"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
  ggsave("data/plots/70_vaccinated_compare_age_clinic.png",width=16, height = 9)  









abs.table.clinic <- vacc.effect.age.ICU[ -c(2:28,34:38)]
abs.table.clinic <- abs.table.clinic[abs.table.clinic$Datum >"2020-10-15",]


key <- "Datum"
value = "test"
abs.table.clinic.long <- gather(abs.table.clinic, key, value, 2:6)
abs.table.clinic.long$key <- as.factor(abs.table.clinic.long$key)




ggplot(abs.table.clinic.long, aes(x=Datum, y= value, color = key))+
  geom_line( size=2 )+
  
  #  scale_y_continuous(labels = percent)+
  scale_x_date(date_breaks = "1 months",date_labels= format("%b"))+
  # limits = as.Date(c("2021-01-6", NA)))+
  
  #coord_cartesian(expand = FALSE)+
  theme_classic()+
  xlab("")+
  ylab("")+
  
  #scale_fill_manual( values=c("#5c146e", "#fca50a", "darkgreen", "#dd513a"), labels=c("zorginstellingen", "GGD'en","Huisartsen", "ziekenhuizen" ))+
  scale_color_brewer(palette = "RdYlBu", labels=c("50-69", "70-79","80-84", "85+", "50-" ))+
  
  # scale_color_manual( values=c("#1F968BFF", "#481567FF"), labels=c("besmettingen totaal", "besmettingen verpleeghuizen" ))+
  
  labs(title = "Opnames Kliniek naar leeftijd",
       #subtitle = "Blauw = Nieuwe besmettingen verpleeghuis \n Rood = Dagelijkse cijfers besmettingen",
       caption = paste("Bron: NICE | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(legend.position = "right",  #c(0.5, 0.925),
        legend.background = element_rect(fill="#DAE3F3",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=27, face="bold"))+
  
  theme(
    plot.background = element_rect(fill = "#DAE3F3"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#DAE3F3", colour = "#DAE3F3"),
    plot.title = element_text(hjust = 0.5,size = 40,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.ticks = element_line(colour = "#DAE3F3", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#DAE3F3"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
  ggsave("data/plots/70_vaccinated_compare_age_clinic_abs.png",width=16, height = 9)  





abs.table.clinic.comp <- vacc.effect.age.ICU[ -c(2:28,34:38)]





#abs.table.clinic.comp.long


#abs.table.clinic.comp.long <- abs.table.clinic.comp %>% mutate(age_grouping = case_when(str_detect(Agegroup, "0-9") ~ '0-9', 
 #                                                                  str_detect(Agegroup, "10-19") ~ '10-19',
  #                                                                 str_detect(Agegroup, "20-29") ~ '20-39',
   #                                                                str_detect(Agegroup, "30-39") ~ '20-39',
    #                                                               str_detect(Agegroup, "40-49") ~ '40-59',
     #                                                              str_detect(Agegroup, "50-59") ~ '40-59',
      #                                                             str_detect(Agegroup, "60-69") ~ '60-79',
       #                                                            str_detect(Agegroup, "70-79") ~ '60-79',
        #                                                           str_detect(Agegroup, "80-89") ~ '80-89',
         #                                                          str_detect(Agegroup, "90+") ~ '90+',))

#casus.working <-count(casus.working,date,age_grouping)

#Take rolling 7-day averages
#casus.working <- casus.working %>% 
#  group_by(age_grouping) %>% 
#  arrange(date) %>% 
#  mutate(cases_avg=roll_mean(n, 7, align="right", fill=0))

#dag<-strftime(Sys.Date()-1)

#casus.working <- casus.working[casus.working$date>"2020-02-29"&casus.working$date<dag,]
#casus.working$date <- as.Date(casus.working$date)


#draw_key_polygon3 <- function(data, params, size) {
 # lwd <- min(data$size, min(size) / 4)
 # 
 # grid::rectGrob(
#    width = grid::unit(0.6, "npc"),
#    height = grid::unit(0.6, "npc"),
#    gp = grid::gpar(
#col = data$colour,
#      fill = alpha(data$fill, data$alpha),
#      lty = data$linetype,
#      lwd = lwd * .pt,
#      linejoin = "mitre"
#    ))
#}
#GeomBar$draw_key = draw_key_polygon3


#### PLOT  onderlinge verhouding ####

#ggplot(casus.working, aes(date,cases_avg,fill=age_grouping))+
#  geom_bar(stat="identity", position=position_fill(), width=1) + scale_y_reverse() +
  
  
  

#abs.table.clinic <- vacc.effect.age.ICU[ -c(2:28,34:38)]
#abs.table.clinic <- abs.table.clinic[abs.table.clinic$Datum >"2020-10-15",]


#key <- "Datum"
#value = "test"
#abs.table.clinic.long <- gather(abs.table.clinic, key, value, 2:6)
#abs.table.clinic.long$key <- as.factor(abs.table.clinic.long$key)




#ggplot(abs.table.clinic, aes(x=Datum, y= value, color = key))+
#  geom_line( size=2 )+
  
  #  scale_y_continuous(labels = percent)+
#  scale_x_date(date_breaks = "1 months",date_labels= format("%b"))+
  # limits = as.Date(c("2021-01-6", NA)))+
  
  #coord_cartesian(expand = FALSE)+
 # theme_classic()+
#  xlab("")+
 # ylab("")+
  
  #scale_fill_manual( values=c("#5c146e", "#fca50a", "darkgreen", "#dd513a"), labels=c("zorginstellingen", "GGD'en","Huisartsen", "ziekenhuizen" ))+
 # scale_color_brewer(palette = "RdYlBu", labels=c("zorginstellingen", "GGD'en","Huisartsen", "ziekenhuizen","test" ))+
  
  # scale_color_manual( values=c("#1F968BFF", "#481567FF"), labels=c("besmettingen totaal", "besmettingen verpleeghuizen" ))+
  
 # labs(title = "Opnames Kliniek naar leeftijd",
       #subtitle = "Blauw = Nieuwe besmettingen verpleeghuis \n Rood = Dagelijkse cijfers besmettingen",
#       caption = paste("Bron: NICE | Plot: @YorickB  | ",Sys.Date()))+
  
  #theme(#legend.position = "none",   # no legend
  #  legend.title = element_blank(),  ## legend title
  #  legend.position="top",
  #  legend.direction = "vertical",
  #  legend.background = element_rect(fill="#f5f5f5", size=0.5, linetype="solid"))+
  
 # theme(
#    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
#    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
#    plot.title = element_text(hjust = 0.5,size = 40,face = "bold"),
#    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
#    axis.text = element_text(size=14,color = "black",face = "bold"),
#    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
#    axis.ticks.length = unit(0.5, "cm"),
#    axis.line = element_line(colour = "#F5F5F5"),
#    panel.grid.major.x = element_blank(),
#  panel.grid.minor.x = element_blank(),
 #   panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))
  
 # ggsave("data/99_vaccinated_compare_age_clinic_abs-22.png",width=16, height = 9)  


