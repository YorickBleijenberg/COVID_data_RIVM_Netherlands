library(tidyverse)
#require(dplyr)
library(zoo)


vacc.effect.age.file <- paste0("https://raw.githubusercontent.com/mzelst/covid-19/master/data-nice/age/leeftijdsverdeling_datum_Klinisch_IC_long.csv")
vacc.effect.age <-read.csv(vacc.effect.age.file,sep=",", check.names = FALSE)
vacc.effect.age$Datum <- as.Date(vacc.effect.age$Datum)

#### select type

type.of.data = "IC"


vacc.effect.age.ICU <- (vacc.effect.age  %>% filter(Type == type.of.data & Datum > "2020-09-01")) # & Datum < "2021-02-01"))  #  Klinisch

vacc.effect.age.ICU <- vacc.effect.age.ICU[-c(21), ]  

vacc.effect.age.ICU$young = vacc.effect.age.ICU$`20 - 24`+vacc.effect.age.ICU$`25 - 29`+
  vacc.effect.age.ICU$`30 - 34`+vacc.effect.age.ICU$`35 - 39`+vacc.effect.age.ICU$`40 - 44`+
  vacc.effect.age.ICU$`45 - 49`   #  +vacc.effect.age.ICU$`<20`
vacc.effect.age.ICU$middle = vacc.effect.age.ICU$`50 - 54` + vacc.effect.age.ICU$`55 - 59` +
                            vacc.effect.age.ICU$`60 - 64` + vacc.effect.age.ICU$`65 - 69`
vacc.effect.age.ICU$old  =  vacc.effect.age.ICU$`70 - 74` + vacc.effect.age.ICU$`75 - 79`
vacc.effect.age.ICU$old80  =  vacc.effect.age.ICU$`80 - 84` +  vacc.effect.age.ICU$`85 - 89` +vacc.effect.age.ICU$`>90`
#vacc.effect.age.ICU$old85p  = 

#vacc.effect.age.ICU$old85  =  vacc.effect.age.ICU$`85 - 89`
#vacc.effect.age.ICU$old90  =  vacc.effect.age.ICU$`>90`

#vacc.effect.age.ICU <- vacc.effect.age.ICU[ -c(2)]




vacc.effect.age.ICU <- (vacc.effect.age.ICU %>%  mutate(young.Change  = young  - lag(young, default = young[1])) )
vacc.effect.age.ICU <- (vacc.effect.age.ICU %>%  mutate(middle.Change = middle - lag(middle, default = middle[1])) )
vacc.effect.age.ICU <- (vacc.effect.age.ICU %>%  mutate(old.Change    = old    - lag(old, default = old[1])) )
vacc.effect.age.ICU <- (vacc.effect.age.ICU %>%  mutate(old80.Change  = old80  - lag(old80, default = old80[1])) )
#vacc.effect.age.ICU <- (vacc.effect.age.ICU %>%  mutate(old85p.Change  = old85p  - lag(old85p, default = old85p[1])) )
#vacc.effect.age.ICU <- (vacc.effect.age.ICU %>%  mutate(old90.Change  = old90  - lag(old90, default = old90[1])) )


vacc.effect.age.ICU$MA.young.Change   <- rollmeanr(vacc.effect.age.ICU$young.Change  , 7, fill = 0)
vacc.effect.age.ICU$MA.middle.Change  <- rollmeanr(vacc.effect.age.ICU$middle.Change , 7, fill = 0)
vacc.effect.age.ICU$MA.old.Change     <- rollmeanr(vacc.effect.age.ICU$old.Change    , 7, fill = 0)
vacc.effect.age.ICU$MA.old80.Change   <- rollmeanr(vacc.effect.age.ICU$old80.Change  , 7, fill = 0)
#vacc.effect.age.ICU$MA.old85p.Change   <- rollmeanr(vacc.effect.age.ICU$old85p.Change  , 7, fill = 0)
#vacc.effect.age.ICU$MA.old90.Change   <- rollmeanr(vacc.effect.age.ICU$old90.Change  , 7, fill = 0)


maxClinyoung <- 5.142857  # max(vacc.effect.age.ICU$MA.young.Change, na.rm = TRUE)
maxClinMid   <- 29  # max(vacc.effect.age.ICU$MA.middle.Change, na.rm = TRUE)
maxClinOld   <- 19.28571  # max(vacc.effect.age.ICU$MA.old.Change, na.rm = TRUE)
maxClin80    <- 2.1428571   # max(vacc.effect.age.ICU$MA.old80.Change, na.rm = TRUE) 2.1428571 - 3.142857 / 3.2857143
#maxClin85p    <- max(vacc.effect.age.ICU$MA.old85p.Change, na.rm = TRUE)
#maxClin90    <- max(vacc.effect.age.ICU$MA.old90.Change, na.rm = TRUE)


vacc.effect.age.ICU$MA.young.Change.relative  <- vacc.effect.age.ICU$MA.young.Change /maxClinyoung
vacc.effect.age.ICU$MA.middle.Change.relative <- vacc.effect.age.ICU$MA.middle.Change/maxClinMid
vacc.effect.age.ICU$MA.old.Change.relative    <- vacc.effect.age.ICU$MA.old.Change   /maxClinOld
vacc.effect.age.ICU$MA.old80.Change.relative  <- vacc.effect.age.ICU$MA.old80.Change /maxClin80
#vacc.effect.age.ICU$MA.old85p.Change.relative  <- vacc.effect.age.ICU$MA.old85p.Change /maxClin85p
#vacc.effect.age.ICU$MA.old90.Change.relative  <- vacc.effect.age.ICU$MA.old90.Change /maxClin90


relative.table.clinic <- vacc.effect.age.ICU[ -c(2:30)]
relative.table.clinic <- relative.table.clinic[relative.table.clinic$Datum >"2020-10-15",]


key <- "Datum"
value <- "test"
relative.table.clinic.long <- gather(relative.table.clinic, key, value, 2:5)

relative.table.clinic.long$key <- as.factor(relative.table.clinic.long$key)

relative.table.clinic.long$key <- relevel(relative.table.clinic.long$key, "MA.young.Change.relative")



#######   

ggplot(relative.table.clinic.long, aes(x=Datum, y= value, color = key))+
  geom_line( size=3 )+
  
  scale_y_continuous(labels = percent)+
  scale_x_date(date_breaks = "1 months",date_labels= format("%b"))+
  # limits = as.Date(c("2021-01-6", NA)))+
  
  #coord_cartesian(expand = FALSE)+
  theme_classic()+
  xlab("")+
  ylab("")+
  
   scale_color_brewer(palette = "RdYlBu", labels=c("50-", "50-69", "70-79","80+" ))+  
  
  #scale_fill_manual( values=c("#5c146e", "#fca50a", "darkgreen", "#dd513a"), labels=c("zorginstellingen", "GGD'en","Huisartsen", "ziekenhuizen" ))+
  #scale_color_brewer(palette = "RdYlBu", labels=c("zorginstellingen", "GGD'en","Huisartsen", "ziekenhuizen" ))+
  
 # scale_color_manual( values=c("#1F968BFF", "#481567FF"), labels=c("besmettingen totaal", "besmettingen verpleeghuizen" ))+
  
  labs(title = "Opnames IC naar leeftijd",
       subtitle = "100% = piek van de wintergolf",
       caption = paste("Bron: NICE | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(legend.position = "right",  #c(0.5, 0.925),
        legend.background = element_rect(fill="#DAE3F3",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=27, face="bold"))+
  
  theme(
    plot.background = element_rect(fill = "#DAE3F3"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#DAE3F3", colour = "#DAE3F3"),
    plot.title = element_text(hjust = 0.5,size = 40,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,size = 20,color = "black", face = "italic"),
    axis.text = element_text(size=20,color = "black",face = "bold"),
    axis.ticks = element_line(colour = "#DAE3F3", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#DAE3F3"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))+
  
ggsave("data/plots/71_vaccinated_compare_age_IC.png",width=16, height = 9)  









abs.table.clinic <- vacc.effect.age.ICU[ -c(2:26,31:34)]
abs.table.clinic <- abs.table.clinic[abs.table.clinic$Datum >"2020-10-15",]



key <- "Datum"
value = "test"
abs.table.clinic.long <- gather(abs.table.clinic, key, value, 2:5)
abs.table.clinic.long$key <- as.factor(abs.table.clinic.long$key)


abs.table.clinic.long$key <- relevel(abs.table.clinic.long$key, "MA.young.Change")





ggplot(abs.table.clinic.long, aes(x=Datum, y= value, color = key))+
  geom_line( size=3 )+
  
  #  scale_y_continuous(labels = percent)+
  scale_x_date(date_breaks = "1 months",date_labels= format("%b"))+
  # limits = as.Date(c("2021-01-6", NA)))+
  
  #coord_cartesian(expand = FALSE)+
  theme_classic()+
  xlab("")+
  ylab("")+
  
  scale_color_brewer(palette = "RdYlBu" , labels=c("50-","50-69", "70-79","80+" ))+  
  
  #scale_fill_manual( values=c("#5c146e", "#fca50a", "darkgreen", "#dd513a"), labels=c("zorginstellingen", "GGD'en","Huisartsen", "ziekenhuizen" ))+
  #scale_color_brewer(palette = "RdYlBu", labels=c("zorginstellingen", "GGD'en","Huisartsen", "ziekenhuizen" ))+
  
  # scale_color_manual( values=c("#1F968BFF", "#481567FF"), labels=c("besmettingen totaal", "besmettingen verpleeghuizen" ))+
  
  labs(title = "Opnames IC naar leeftijd",
       subtitle = "Naar dag van rapportage",
       caption = paste("Bron: NICE | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(legend.position = "right",  #c(0.5, 0.925),
        legend.background = element_rect(fill="#DAE3F3",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=27, face="bold"))+
  
  theme(
    plot.background = element_rect(fill = "#DAE3F3"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#DAE3F3", colour = "#DAE3F3"),
    plot.title =     element_text(hjust = 0.5, size = 40, face = "bold"),
    plot.subtitle =  element_text(hjust = 0.5, size = 20, color = "black", face = "italic"),
    axis.text = element_text(size=20,color = "black",face = "bold"),
    axis.ticks = element_line(colour = "#DAE3F3", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#DAE3F3"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))+
  
  ggsave("data/plots/71_vaccinated_compare_age_ICU_abs.png",width=16, height = 9)  


