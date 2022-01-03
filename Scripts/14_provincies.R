library(tidyverse)
library(data.table)
library(zoo)

#library(paletteer)
library(ggrepel)


##### import from RIVM website ####


## RIVM_aantallen_gemeente_cumulatief<-read.csv("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv",sep=";")  #import from RIVM website

## import from local file

File_date_35 <- paste0("data/",format(Sys.Date(), "%Y-%m-%d"),"/",format(Sys.Date(), "%Y-%m-%d"), "_COVID-19_aantallen_gemeente_cumulatief.csv")
RIVM_aantallen_gemeente_cumulatief<-read.csv(File_date_35,sep=";")

## temp store

number_muni_cum <- RIVM_aantallen_gemeente_cumulatief   #File_date_3





FR_a <- intToUtf8(0x00E2)  #Encoding(FR_b) <- "UTF-8"
FR_b <- paste0("Frysl", FR_a,"n")
number_muni_cum$Province <- str_replace(number_muni_cum$Province, "Fryslân", FR_b)  ##fout / goed
number_muni_cum$Province <- str_replace(number_muni_cum$Province, "Friesland", FR_b)  ##fout / goed


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

number_new_prov_short <- number_new_prov[number_new_prov$Date>"2020-06-01"&number_new_prov$Date<=Sys.Date(),]




#### Province plot new  facet ####

ggplot(data = number_new_prov_short, ) + 
  #geom_bar(stat='identity', mapping = aes(x = Date, y = newCases), colour = "gray", size = 2)+
   geom_point(stat='identity', mapping = aes(x = Date, y = newCases), colour = "gray", size = 2)+
  geom_line(mapping = aes(x = Date, y = MAnewCases), colour = "darkred", size =1.5)+
 
  scale_y_continuous(limits = c(0,NA) )+
  
   facet_wrap(~ Province,  scales = "free_y")+
  
  scale_x_date(date_breaks = "2 week", 
               date_labels= format("%d %b"))+
  
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


ggsave("data/20_prov_new-no-color.png",width=16, height = 9)




#### Province plot new EN ####

ggplot(data = number_new_prov_short, ) + 
  geom_bar(stat='identity', mapping = aes(x = Date, y = newCases), colour = "black", size = 2)+
  geom_line(mapping = aes(x = Date, y = MAnewCases), colour = "darkred", size =1.5)+
  
  facet_wrap(~ Province, scales = "free_y")+
  
  theme_bw() + 
  
  xlab("")+ 
  ylab("")+
  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%b"),
               limits = as.Date(c("2021-11-15", NA))
  )+
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

#ggplot(data = number_new_prov_short, mapping = aes(x = Date, y = MAnewCases, fill = factor(Province, levels=c("Drenthe", "Groningen"))))+ 

 #  geom_line(size = 2)+
 # theme_classic()+
  #xlab("")+ 
 # ylab("")+
 # labs(title = "Provincies, Nieuwe gevallen per dag",
#       subtitle = "7 daags voortschrijdend gemiddelde",
#       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
  

 # scale_color_paletteer_d("nord::aurora")

  # scale_colour_brewer(type = "seq", palette = "Spectral")+
  #scale_colour_brewer(palette = "Set1")
  # scale_colour_brewer(palette = "Set3")+
#
#  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
 #        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         # legend.position = "none",   # no legend
#         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
#         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
 #        axis.text = element_text(size=14,color = "black",face = "bold"),
 #        axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
  #       axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
  #       axis.ticks.length = unit(0.5, "cm"),
 #        axis.line = element_line(colour = "#F5F5F5"),
 #        panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
 #        )

#ggsave("data/18_Province_newCasesMA-set3.png",width=16, height = 9)



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

#ggsave("data/18_Province_cumulative_log.png",width=16, height = 9)

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



#### Province plot new NO FACET SCALES ####



today <- Sys.Date()

inwo_prov <- "C:\\Rdir\\data-contstant\\provincies.csv"
prov.inwoners <- read.csv(inwo_prov,sep=";")  
colnames(prov.inwoners) = c("Province", "Prov_getal", "Prov_inwoners")


prov.inwoners$Province <- str_replace(prov.inwoners$Province, "Friesland", FR_b)  ##fout / goed



prov.combi <- merge(number_new_prov_short, prov.inwoners)


prov.combi$phd <- (prov.combi$MAnewCases/prov.combi$Prov_inwoners)*100000


kleur.table.prov  = data.frame()

i=1
while (i < 13)
{
  
  test.data.8 <- prov.combi
  test.data.8 <- test.data.8[test.data.8$Prov_getal == i,]
  
  ff <- last(test.data.8$phd)
  gg <- last(test.data.8$phd,15)
  ii <- head(tail(test.data.8$phd, n=8), n=1)
  hh <- head(tail(test.data.8$phd, n=15), n=1)
  
  
  kleur <- paste("yellow")
  if (((ff > hh+2) | (ff > ii+2))&(ff < ii+5)) {
    kleur <- paste("red")
  } else if (ff > ii+5) {
    kleur <- paste("help")
  } else if (((ff < hh-2) & (ff < ii-2))| (ff<1)) {
    kleur <- paste("green")
  }
  
  kleur.table.prov = rbind(kleur.table.prov, data.frame(Prov_getal=i,kleur=kleur))
  
  i <- i+1
}


prov.combi.all <- merge(prov.combi, kleur.table.prov)







#number_new_prov_short$Province <- as.factor(number_new_prov_short$Province)

ggplot(data = prov.combi.all) + 
  #geom_point(stat='identity', mapping = aes(x = Date, y = phd,), colour = "gray", size = 2)+
  geom_line(mapping = aes(x = Date, y = phd, color = kleur), size =2)+   #, colour = "darkred"

  scale_color_manual(values=c("red", "darkred", "green", "orange"))+
    facet_wrap(~ Province,)+  # scales = "free_y")+

  theme_bw() + 
  xlab("")+ 
  ylab("")+
  
labs(title = "Provincies",
       subtitle = "Nieuwe gevallen per 100.000 inwoners, 7-daags zwevend gemiddelde",
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

ggsave("data/20_prov_phd.png",width=16, height = 9)




#number_new_prov_short$Province <- as.factor(number_new_prov_short$Province)

ggplot(data = prov.combi.all) + 
  geom_point(stat='identity', mapping = aes(x = Date, y = newCases,), colour = "gray", size = 2)+
  geom_line(mapping = aes(x = Date, y = MAnewCases, color = kleur), size =1.5)+   #, colour = "darkred"
  
  scale_color_manual(values=c("green", "darkred", "red", "orange"))+
  facet_wrap(~ Province,)+  # scales = "free_y")+
 
  theme_bw() + 
  xlab("")+ 
  ylab("")+
  
  labs(title = "Provincies",
       subtitle = "Nieuwe gevallen, 7-daags zwevend gemiddelde",
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





require(pals)






ggplot(data = prov.combi.all) + 
  geom_line(mapping = aes(x = Date, y = MAnewCases, color = Province), size =1.5, alpha = 0.8)+   #, colour = "darkred"

 #  scale_color_manual(values=c("green", "darkred", "red", "orange"))+
 #  facet_wrap(~ Province,)+  # scales = "free_y")+
  
  scale_color_manual(values=pals::cols25())+
  
  #scale_color_manual(values=pals::brewer.paired(12))+
  
  theme_bw() + 
  xlab("")+ 
  ylab("")+
  
  labs(title = "Provincies",
       subtitle = "Nieuwe gevallen, 7-daags zwevend gemiddelde",
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        # legend.position = "none",   # no legend
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
         legend.title = element_blank(),
         legend.background = element_rect(fill="#F5F5F5")
  )

ggsave("data/20_prov_new-test.png",width=16, height = 9)


