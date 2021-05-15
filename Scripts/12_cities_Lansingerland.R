library(tidyverse)
library(ggplot2)
library(zoo)


## import from RIVM website

# RIVM_aantallen_gemeente_cumulatief<-read.csv("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv",sep=";")  #import from RIVM website

## import from local file

File_date_3 <- paste0("data/",format(Sys.Date(), "%Y-%m-%d"),"/",format(Sys.Date(), "%Y-%m-%d"), "_COVID-19_aantallen_gemeente_cumulatief.csv")
RIVM_aantallen_gemeente_cumulatief<-read.csv(File_date_3,sep=";")

## temp store

number_muni_cum <- RIVM_aantallen_gemeente_cumulatief   #File_date_3

####   select only the Province, Date of Report and sum of total reported  ####

number_muni_cum_prov <- number_muni_cum[,c("Date_of_report","Municipality_name","Total_reported")]
number_muni_cum_prov$Date_of_report <- as.Date(number_muni_cum_prov$Date_of_report)

number_muni_cum_city_agg <- aggregate(number_muni_cum_prov$Total_reported,     by=list(Date=number_muni_cum_prov$Date_of_report, City = number_muni_cum_prov$Municipality_name ), FUN=sum)





####   spread to select right cities #####

number_cum_city_spread <- spread(number_muni_cum_city_agg, City, x)

number_cum_city_spread_small <- number_cum_city_spread[,c("Date",
                                                          "Amsterdam",
                                                          "Rotterdam")]
  
number_cum_city_gather <- gather(number_cum_city_spread_small, City,  x, -"Date")


#### Calculate the new cases per province per day ####
number_new_city <- number_cum_city_gather
number_new_city  <- (number_new_city %>% group_by(City) %>%   mutate(newCases = x - lag(x)))

#### Calculate the 7 day MA per province per day ####


number_new_city <- number_new_city %>% 
  group_by(City) %>% 
  mutate(MAnewCases = rollapply(newCases, 7, mean, fill = NA, align = "right"))


####  Set date ####

number_new_city <- number_new_city[number_new_city$Date>"2020-07-01"&number_new_city$Date<=Sys.Date(),]



#### City plot new  ####

ggplot(data = number_new_city, ) + 
 # geom_vline(aes(xintercept= as.Date("2021-02-09")), color="black", linetype = "dotted") +
  geom_point(stat='identity', mapping = aes(x = Date, y = newCases), colour = "gray", size = 2)+
  geom_line(mapping = aes(x = Date, y = MAnewCases), colour = "darkred", size =1.5)+
   facet_wrap(~ City, )+ # scales = "free_y")+
  theme_bw() + 
  xlab("")+ 
  ylab("")+
  labs(title = "Nieuwe gevallen",
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
         #axis.line = element_line(colour = "#F5F5F5"),
        
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
         ### facet label custom
         strip.text.x = element_text(size = 13, color = "black"),
         strip.background = element_rect(color="black", fill="gray", size=1.5, linetype="solid"),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
  )

ggsave("data/18_city_new_Lansingerland.png",width=16, height = 9)

