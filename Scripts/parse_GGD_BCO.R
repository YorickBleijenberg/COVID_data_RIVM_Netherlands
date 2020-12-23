require(tabulizer)
require(tidyverse)
require(lubridate)
require(scales) 
library(dplyr)
library(ggforce)

####
####  parse code by mzelst  ###
####


weeknumber <- isoweek(Sys.Date())-1


report <- "https://www.rivm.nl/sites/default/files/2020-12/COVID-19_WebSite_rapport_wekelijks_20201222_1259.pdf"



#### BCO.1 GGD ####


area.table.ggd.total <- locate_areas(report,
                                          pages=c(22))

ggd_bco1 <- extract_tables(report,
                           output = "data.frame",
                           pages = c(22),
                           area = area.table.ggd.total,
                           guess=FALSE)

ggd_bco1 <- do.call(rbind,ggd_bco1)

colnames(ggd_bco1) <- c("setting","aantal.31-9","perc.31-9","aantal.this.week", "perc.this.week")

ggdBCOFile <- paste0("data-dashboards/ggd_bco1_", weeknumber, ".csv")
write.csv(ggd_bco1,file = ggdBCOFile, row.names = F)


#### BCO.2 GGD ####


area.table.ggd.total <- locate_areas(report,
                                     pages=c(23))

ggd_bco2 <- extract_tables(report,
                           output = "data.frame",
                           pages = c(23),
                           area = area.table.ggd.total,
                           guess=FALSE)

ggd_bco2 <- do.call(rbind,ggd_bco2)

colnames(ggd_bco2) <- c("setting","aantal.31-9","perc.31-9","aantal.this.week", "perc.this.week")

ggdBCO2File <- paste0("data-dashboards/ggd_bco2_", weeknumber, ".csv")
write.csv(ggd_bco2,file = ggdBCO2File, row.names = F)



bco_tot <- (ggd_bco1$aantal.this.week[1]+ggd_bco1$aantal.this.week[2]+ggd_bco1$aantal.this.week[3]+ggd_bco1$aantal.this.week[4]+ggd_bco1$aantal.this.week[5])
bco_yes.setting <- (ggd_bco1$aantal.this.week[1])
bco_no.setting <- (ggd_bco1$aantal.this.week[2]+ggd_bco1$aantal.this.week[3]+ggd_bco1$aantal.this.week[4]+ggd_bco1$aantal.this.week[5])
bco.setting.perc <- 1-(bco_no.setting/bco_tot)


ggd_bco3 <- ggd_bco2[-c(2, 4, 15), ]
sum.bco3 <- (sum(ggd_bco3$perc.this.week)/100)

ggd_bco3$percentage.relative <-   (ggd_bco3$aantal.this.week/bco_yes.setting/sum.bco3)
ggd_bco3$percentage.overall <-   (ggd_bco3$percentage.relative/(1/ggd_bco1$perc.this.week[1]))

ggd_bco4 <- ggd_bco1
ggd_bco4$percentage.overall <- 100-ggd_bco1$perc.this.week[1]
ggd_bco4 <- ggd_bco4[-c(2:5),-c(2:5)]


ggd_bco5 <- bind_rows(ggd_bco4,ggd_bco3)
ggd_bco5 <- ggd_bco5 [-c(7:24),-c(3:7)]  ## first argument = number of settings.


sum.other <- 100-sum(ggd_bco5$percentage.overall)
other.bco <- c("Anders", sum.other)
ggd_bco5 <- rbind(ggd_bco5, other.bco)
ggd_bco5$percentage.overall <-  as.double(ggd_bco5$percentage.overall)

ggd_bco5$perc.round <- round(ggd_bco5$percentage.overall,digits=2)

ggd_bco5$setting <- str_replace(ggd_bco5$setting, "Ja, setting vermeld",                          "setting onbeked: community spread")  
ggd_bco5$setting <- str_replace(ggd_bco5$setting, "Vrijetijdsbesteding, zoals sportclub",          "Vrijetijdsbesteding")  
ggd_bco5$setting <- str_replace(ggd_bco5$setting, "Verpleeghuis of woonzorgcentrum voor ouderen4", "Verpleeghuis/woonzorgcentrum")  
ggd_bco5$setting <- str_replace(ggd_bco5$setting, "Thuissituatie \\(huisgenoten en partner, niet",    "Thuissituatie")  
ggd_bco5$setting <- str_replace(ggd_bco5$setting, "Bezoek in de thuissituatie \\(van of bij familie,", "Bezoek, familie/kennis/vrienden")
ggd_bco5$setting <- str_replace(ggd_bco5$setting, "2e lijn gezondheidszorg / ziekenhuis", "ziekenhuis")  

ggd_bco5$Label <- paste(ggd_bco5$setting, paste(ggd_bco5$perc.round,"%"), sep="\n")

ggd_bco5 <- ggd_bco5 %>% 
  mutate(end = 2 * pi * cumsum(percentage.overall)/sum(percentage.overall),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))



ggplot(ggd_bco5) + 
geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                 start = start, end = end, fill = Label)) +
  geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = Label,
                hjust = hjust, vjust = vjust)) +
  coord_fixed() +
  
  scale_x_continuous(limits = c(-2, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1, 1.1),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL)+
  
  #scale_fill_brewer(palette="Set1")+
  scale_fill_manual(values = c("#4daf4a","#ff7f00","#ffff33","#e41a1c","#377eb8","#a65628","#984ea3", "#a65628"))+
  
  #e41a1c - red --
  #ff7f00 - orange---
  #377eb8 - blue----
  #a65628 - brown
  #4daf4a - green---
  #ffff33 - yellow
  #984ea3 -purple
  
  
  
  labs(title = "Setting van mogelijk besmetting",
         subtitle = "BCO, gevallen afgelopen week",
       #                  "semi-lockdown op 14 oktober\n",
       #                  "Herfstvakantie midden/zuid: 17-25 oktober"),
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
           panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
           plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
           plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
           legend.position = "none",
           axis.text = element_text(size=14,color = "black",face = "bold"),
           axis.text.y = element_text(face="bold", color="black", size=12),  #, angle=45),
           axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
           axis.ticks.length = unit(0.5, "cm"),
          # axis.line = element_line(colour = "black"),
  )

ggsave("data/86_setting_gg.png",width=16, height = 9)

