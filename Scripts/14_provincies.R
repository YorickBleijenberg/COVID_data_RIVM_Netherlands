library(tidyverse)
library(data.table)
library(zoo)

#library(paletteer)
#library(ggrepel)


##### import from RIVM website ####


## RIVM_aantallen_gemeente_cumulatief<-read.csv("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv",sep=";")  #import from RIVM website

## import from local file

File_date_35 <- paste0("data/",format(Sys.Date(), "%Y-%m-%d"),"/",format(Sys.Date(), "%Y-%m-%d"), "_COVID-19_aantallen_gemeente_cumulatief.csv")
RIVM_aantallen_gemeente_cumulatief<-read.csv(File_date_35,sep=";")

## temp store

number_muni_cum <- RIVM_aantallen_gemeente_cumulatief   #File_date_3


####   select only the Province, Date of Report and sum of total reported ####

number_muni_cum_prov <- number_muni_cum[,c("Date_of_report","Province","Total_reported")]
number_muni_cum_prov$Date_of_report <- as.Date(number_muni_cum_prov$Date_of_report)

#### Aggregate the columns to make a nice plot #####

number_muni_cum_prov_agg <- aggregate(number_muni_cum_prov$Total_reported, by=list(Date=number_muni_cum_prov$Date_of_report, Province = number_muni_cum_prov$Province ), FUN=sum)


#### Calculate the new cases per province per day ####
number_new_prov <- number_muni_cum_prov_agg
number_new_prov  <- (number_new_prov %>% group_by(Province) %>%   mutate(newCases = x - lag(x)))

#### Calculate the 7 day MA per province per day ####


number_new_prov <- number_new_prov %>% 
  group_by(Province) %>% 
  mutate(MAnewCases = rollapply(newCases, 7, mean, fill = NA, align = "right"))


####  Set date ####

number_new_prov_short <- number_new_prov[number_new_prov$Date>"2020-07-01"&number_new_prov$Date<=Sys.Date(),]




#### Province plot new  facet ####

ggplot(data = number_new_prov_short, ) + 
  #geom_bar(stat='identity', mapping = aes(x = Date, y = newCases), colour = "gray", size = 2)+
   geom_point(stat='identity', mapping = aes(x = Date, y = newCases), colour = "gray", size = 2)+
  geom_line(mapping = aes(x = Date, y = MAnewCases), colour = "darkred", size =1.5)+
  facet_wrap(~ Province,  scales = "free_y")+
  theme_bw() + 
  xlab("")+ 
  ylab("")+
  labs(title = "Provincies",
       subtitle = "Nieuwe gevallen, 7-daags zwevend gemiddelde  (Y-as wisselt)",
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         legend.position = "none",   # no legend
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=12),  #, angle=45),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
        # axis.line = element_line(colour = "#F5F5F5"),
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
         ### facet label custom
         strip.text.x = element_text(size = 13, color = "black"),
         strip.background = element_rect(color="black", fill="gray", size=1.5, linetype="solid"),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
  )


ggsave("data/20_prov_new.png",width=16, height = 9)




#### Province plot new EN ####

ggplot(data = number_new_prov_short, ) + 
  geom_point(stat='identity', mapping = aes(x = Date, y = newCases), colour = "gray", size = 2)+
  geom_line(mapping = aes(x = Date, y = MAnewCases), colour = "darkred", size =1.5)+
  facet_wrap(~ Province, scales = "free_y")+
  
  theme_bw() + 
  
  xlab("")+ 
  ylab("")+
  labs(title = "Provinces",
       subtitle = "New cases, 7 day moving average  (Y-axes changes)",
       caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         legend.position = "none",   # no legend
         
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=12),  #, angle=45),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),

         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
         
         ### facet label custom
         strip.text.x = element_text(size = 13, color = "black"),
         strip.background = element_rect(color="black", fill="gray", size=1.5, linetype="solid"),
        
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
         )


ggsave("data/20_EN_prov_new.png",width=16, height = 9)



##### plot province new cases ####

#Zuid-Holland
#Friesland
#Gelderland
#Limburg
#Overijssel
#Utrecht

#,"Noord-Holland","Noord-Brabant","Groningen","Zeeland","Flevoland"

ggplot(data = number_new_prov_short, mapping = aes(x = Date, y = MAnewCases, fill = factor(Province, levels=c("Drenthe", "Groningen"))))+ 

  geom_line(size = 2)+
  theme_classic()+
  xlab("")+ 
  ylab("")+
  labs(title = "Provincies, Nieuwe gevallen per dag",
       subtitle = "7 daags voortschrijdend gemiddelde",
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
  

 # scale_color_paletteer_d("nord::aurora")

  # scale_colour_brewer(type = "seq", palette = "Spectral")+
  #scale_colour_brewer(palette = "Set1")
  # scale_colour_brewer(palette = "Set3")+


  
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         # legend.position = "none",   # no legend
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
         axis.line = element_line(colour = "#F5F5F5"),
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
         )

ggsave("data/18_Province_newCasesMA-set3.png",width=16, height = 9)



##### plot province cumulative ####

ggplot(data = number_muni_cum_prov_agg, mapping = aes(x = Date, y = x, color = Province)) + 
  geom_line(size = 2)+
  theme_classic()+
  xlab("")+ 
  ylab("")+
  labs(title = "Provincies, cumulatief",
       #subtitle = "met 7 daags voortschrijdend gemiddelde",
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        # legend.position = "none",   # no legend
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
         axis.line = element_line(colour = "#F5F5F5"),
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))

ggsave("data/18_Province_cumulative_log.png",width=16, height = 9)

####  plot province cumulative LOGARIMIC  ####
ggplot(data = number_muni_cum_prov_agg, mapping = aes(x = Date, y = x, color = Province)) + 
  geom_line(size = 2)+
  theme_classic()+
  xlab("")+ 
  ylab("")+
  scale_y_continuous(trans='log2')+
  labs(title = "Provincies, cumulatief",
       subtitle = "logaritmisch",
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         # legend.position = "none",   # no legend
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
         axis.line = element_line(colour = "#F5F5F5"),
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))

# ggsave("data/18_Province_cumulative-log.png",width=16, height = 9)






#### Province plot new NO FACET SCALES ####

ggplot(data = number_new_prov_short, ) + 
  geom_point(stat='identity', mapping = aes(x = Date, y = newCases), colour = "gray", size = 2)+
  geom_line(mapping = aes(x = Date, y = MAnewCases), colour = "darkred", size =1.5)+
  facet_wrap(~ Province)+  #  ,  scales = "free_y")+
  theme_bw() + 
  xlab("")+ 
  ylab("")+
  labs(title = "Provincies",
       subtitle = "Nieuwe gevallen, 7-daags zwevend gemiddelde  (Y-as wisselt)",
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         legend.position = "none",   # no legend
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=12),  #, angle=45),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
         # axis.line = element_line(colour = "#F5F5F5"),
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
         ### facet label custom
         strip.text.x = element_text(size = 13, color = "black"),
         strip.background = element_rect(color="black", fill="gray", size=1.5, linetype="solid"),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
  )


ggsave("data/20_prov_new-fixed-scale.png",width=16, height = 9)

