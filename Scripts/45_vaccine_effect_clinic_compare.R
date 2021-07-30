library(tidyverse)
#require(dplyr)
library(zoo)


vacc.effect.age.file <- paste0("https://raw.githubusercontent.com/mzelst/covid-19/master/data-nice/age/leeftijdsverdeling_datum_Klinisch_IC_long.csv")
vacc.effect.age <-read.csv(vacc.effect.age.file,sep=",", check.names = FALSE)
vacc.effect.age$Datum <- as.Date(vacc.effect.age$Datum)

#### select type

type.of.data = "Klinisch"


vacc.effect.age.ICU <- (vacc.effect.age  %>% filter(Type == type.of.data & Datum > "2020-09-15")) # & Datum < "2021-02-01"))  #  Klinisch

# vacc.effect.age.ICU <- (vacc.effect.age  %>% filter(Type == type.of.data & Datum > "2020-12-01" & Datum < "2021-02-01")) #peak winter



vacc.effect.age.ICU$young =  vacc.effect.age.ICU$`<20`     + vacc.effect.age.ICU$`20 - 24` + vacc.effect.age.ICU$`25 - 29`
vacc.effect.age.ICU$three      =  vacc.effect.age.ICU$`30 - 34` + vacc.effect.age.ICU$`35 - 39`
vacc.effect.age.ICU$four       =  vacc.effect.age.ICU$`40 - 44` + vacc.effect.age.ICU$`45 - 49`   
vacc.effect.age.ICU$fifty      =  vacc.effect.age.ICU$`50 - 54` + vacc.effect.age.ICU$`55 - 59`
vacc.effect.age.ICU$sixty      =  vacc.effect.age.ICU$`60 - 64` + vacc.effect.age.ICU$`65 - 69`
vacc.effect.age.ICU$seven      =  vacc.effect.age.ICU$`70 - 74` + vacc.effect.age.ICU$`75 - 79`
vacc.effect.age.ICU$old80      =  vacc.effect.age.ICU$`80 - 84` + vacc.effect.age.ICU$`85 - 89` +vacc.effect.age.ICU$`>90`

#vacc.effect.age.ICU <- vacc.effect.age.ICU[ -c(2)]

vacc.effect.age.ICU <- (vacc.effect.age.ICU %>%  mutate(young.Change  = young  - lag(young, default = young[1])) )
vacc.effect.age.ICU <- (vacc.effect.age.ICU %>%  mutate(three.Change = three - lag(three, default = three[1])) )
vacc.effect.age.ICU <- (vacc.effect.age.ICU %>%  mutate(four.Change = four - lag(four, default = four[1])) )
vacc.effect.age.ICU <- (vacc.effect.age.ICU %>%  mutate(fifty.Change = fifty - lag(fifty, default = fifty[1])) )
vacc.effect.age.ICU <- (vacc.effect.age.ICU %>%  mutate(sixty.Change = sixty - lag(sixty, default = sixty[1])) )
vacc.effect.age.ICU <- (vacc.effect.age.ICU %>%  mutate(seven.Change    = seven    - lag(seven, default = seven[1])) )
vacc.effect.age.ICU <- (vacc.effect.age.ICU %>%  mutate(old80.Change  = old80  - lag(old80, default = old80[1])) )


vacc.effect.age.ICU$MA.young.Change   <- rollmeanr(vacc.effect.age.ICU$young.Change  , 7, fill = 0)
vacc.effect.age.ICU$MA.three.Change  <- rollmeanr(vacc.effect.age.ICU$three.Change , 7, fill = 0)
vacc.effect.age.ICU$MA.four.Change  <- rollmeanr(vacc.effect.age.ICU$four.Change , 7, fill = 0)
vacc.effect.age.ICU$MA.fifty.Change  <- rollmeanr(vacc.effect.age.ICU$fifty.Change , 7, fill = 0)
vacc.effect.age.ICU$MA.sixty.Change  <- rollmeanr(vacc.effect.age.ICU$sixty.Change , 7, fill = 0)
vacc.effect.age.ICU$MA.seven.Change     <- rollmeanr(vacc.effect.age.ICU$seven.Change    , 7, fill = 0)
vacc.effect.age.ICU$MA.old80.Change   <- rollmeanr(vacc.effect.age.ICU$old80.Change  , 7, fill = 0)

maxClinyoung <- 10.14285 # max(vacc.effect.age.ICU$MA.young.Change, na.rm = TRUE)
maxClinThree <- 9.428571 # max(vacc.effect.age.ICU$MA.three.Change, na.rm = TRUE)
maxClinFour  <- 16.28571 # max(vacc.effect.age.ICU$MA.four.Change, na.rm = TRUE)
maxClinFive  <- 37.42857 # max(vacc.effect.age.ICU$MA.fifty.Change, na.rm = TRUE)
maxClinSix   <- 50.28571 # max(vacc.effect.age.ICU$MA.sixty.Change, na.rm = TRUE)
maxClinSeven <- 79.28571 # max(vacc.effect.age.ICU$MA.seven.Change, na.rm = TRUE)
maxClin80    <- 70.85714 # max(vacc.effect.age.ICU$MA.old80.Change, na.rm = TRUE)  # 2.1428571 - 3.142857 / 3.2857143


vacc.effect.age.ICU$MA.young.Change.relative <- vacc.effect.age.ICU$MA.young.Change /maxClinyoung
vacc.effect.age.ICU$MA.three.Change.relative <- vacc.effect.age.ICU$MA.three.Change /maxClinThree
vacc.effect.age.ICU$MA.four.Change.relative  <- vacc.effect.age.ICU$MA.four.Change  /maxClinFour
vacc.effect.age.ICU$MA.fifty.Change.relative <- vacc.effect.age.ICU$MA.fifty.Change /maxClinFive
vacc.effect.age.ICU$MA.sixty.Change.relative <- vacc.effect.age.ICU$MA.sixty.Change /maxClinSix
vacc.effect.age.ICU$MA.seven.Change.relative <- vacc.effect.age.ICU$MA.seven.Change /maxClinSeven
vacc.effect.age.ICU$MA.old80.Change.relative <- vacc.effect.age.ICU$MA.old80.Change /maxClin80



relative.table.clinic <- vacc.effect.age.ICU[ -c(2:39)]
relative.table.clinic <- relative.table.clinic[relative.table.clinic$Datum >"2020-10-15",]



key <- "Datum"
value <- "test"
relative.table.clinic.long <- gather(relative.table.clinic, key, value, 2:8)

relative.table.clinic.long$key <- as.factor(relative.table.clinic.long$key)

relative.table.clinic.long$key <- relevel(relative.table.clinic.long$key, "MA.young.Change.relative")

# relative.table.clinic.long$key <- as.factor(relative.table.clinic.long$key, levels = c("50-69", "70-79","80-84", "85+", "50-"))
                                              
                                              #c("MA.young.Change.relative", "MA.middle.Change.relative","MA.old.Change.relative","MA.old80.Change.relative","MA.old85p.Change.relative"))



#######   

ggplot(relative.table.clinic.long, aes(x=Datum, y= value, color = factor(key, levels=c("MA.young.Change.relative",
                                                                                       "MA.three.Change.relative",
                                                                                       "MA.four.Change.relative",
                                                                                       "MA.fifty.Change.relative",
                                                                                       "MA.sixty.Change.relative",
                                                                                       "MA.seven.Change.relative",
                                                                                       "MA.old80.Change.relative"))))+
  
  geom_line( size=2 )+
  
  scale_y_continuous(labels = percent, limits = c(0,1.6))+
  scale_x_date( limits=as.Date(c("2020-11-15", NA)), date_breaks = "1 months",date_labels= format("%b"))+
  # limits = as.Date(c("2021-01-6", NA)))+
  
  
  
  
  #coord_cartesian(expand = FALSE)+
  theme_classic()+
  xlab("")+
  ylab("")+
  
  scale_color_manual( values=c("#f8766d", "#c49a00","#53b400","#00c094","#00b6eb","#a58aff","#fb61d7"), labels=c("30-","30-39", "40-49","50-59","60-69","70-79","80+"))+
  
  #scale_fill_manual( values=c("#5c146e", "#fca50a", "darkgreen", "#dd513a"), labels=c("zorginstellingen", "GGD'en","Huisartsen", "ziekenhuizen" ))+
  #scale_color_brewer(palette = "RdYlBu", labels=c("zorginstellingen", "GGD'en","Huisartsen", "ziekenhuizen" ))+

  # scale_color_brewer(palette = "RdYlBu", labels=c("50-", "50-69", "70-79","80-84", "85+"  ))+  
 
  # scale_color_manual( values=c("#1F968BFF", "#481567FF"), labels=c("besmettingen totaal", "besmettingen verpleeghuizen" ))+
  
  labs(title = "Opnames Kliniek naar leeftijd",
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
    plot.subtitle =  element_text(hjust=0.5, size=20,color = "black", face = "italic"),
    axis.text = element_text(size=20,color = "black",face = "bold"),
    axis.ticks = element_line(colour = "#DAE3F3", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#DAE3F3"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))+
  
  ggsave("data/plots/70_vaccinated_compare_age_clinic.png",width=16, height = 9)  












abs.table.clinic <- vacc.effect.age.ICU[ -c(2:32,40:46)]
abs.table.clinic <- abs.table.clinic[abs.table.clinic$Datum >"2020-10-15",]





key <- "Datum"
value = "test"
abs.table.clinic.long <- gather(abs.table.clinic, key, value, 2:8)
abs.table.clinic.long$key <- as.factor(abs.table.clinic.long$key)

abs.table.clinic.long$key <- relevel(abs.table.clinic.long$key, "MA.young.Change")



ggplot(abs.table.clinic.long, aes(x=Datum, y= value, color = factor(key, levels=c("MA.young.Change",
                                                                                  "MA.three.Change",
                                                                                  "MA.four.Change",
                                                                                  "MA.fifty.Change",
                                                                                  "MA.sixty.Change", 
                                                                                  "MA.seven.Change",
                                                                                  "MA.old80.Change"))))+
  geom_line( size=2 )+
  
  #  scale_y_continuous(labels = percent)+
  scale_x_date(limits=as.Date(c("2021-05-15", NA)), date_breaks = "1 week",date_labels= format("%d %b"))+
  # limits = as.Date(c("2021-01-6", NA)))+
  
  scale_y_continuous(limits = c(0,15), breaks=  c(0,2,4,6,8,10,12,14))+
  
  
  #coord_cartesian(expand = FALSE)+
  theme_classic()+
  xlab("")+
  ylab("")+
  
  scale_color_manual( values=c("#f8766d", "#c49a00","#53b400","#00c094","#00b6eb","#a58aff","#fb61d7"), labels=c("30-","30-39", "40-49","50-59","60-69","70-79","80+"))+
  
  
  #scale_fill_manual( values=c("#5c146e", "#fca50a", "darkgreen", "#dd513a"), labels=c("zorginstellingen", "GGD'en","Huisartsen", "ziekenhuizen" ))+
 # scale_color_brewer(palette = "RdYlBu", labels=c("50-", "50-69", "70-79","80-84", "85+" ))+
  
  # scale_color_manual( values=c("#1F968BFF", "#481567FF"), labels=c("besmettingen totaal", "besmettingen verpleeghuizen" ))+
  
  labs(title = "Opnames Kliniek naar leeftijd",
       subtitle = "Lopend 7-daags gemiddelde, naar dag van rapportage",
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
  
  ggsave("data/plots/70_vaccinated_compare_age_clinic_abs.png",width=16, height = 9)  







