library(tidyverse)
library(rtweet)
library(data.table)
library(zoo)

### Apple mobility ###

### Apple update at 21:00 CET ###


#### authenticate Twitter account ####

source("C:\\Rdir\\Rscripts\\03A_TwitterAuthentication.r")

###
###  download file from: https://covid19.apple.com/mobility
###
###  https://covid19-static.cdn-apple.com/covid19-mobility-data/2019HotfixDev29/v3/en-us/applemobilitytrends-2020-11-03.csv
###  https://covid19-static.cdn-apple.com/covid19-mobility-data/2019HotfixDev31/v3/en-us/applemobilitytrends-2020-11-05.csv
###  https://covid19-static.cdn-apple.com/covid19-mobility-data/2019HotfixDev32/v3/en-us/applemobilitytrends-2020-11-06.csv

##   https://covid19-static.cdn-apple.com/covid19-mobility-data/2019HotfixDev36/v3/en-us/applemobilitytrends-2020-11-10.csv
##   https://covid19-static.cdn-apple.com/covid19-mobility-data/2021HotfixDev9/v3/en-us/applemobilitytrends-2020-11-12.csv

##   https://covid19-static.cdn-apple.com/covid19-mobility-data/2021HotfixDev11/v3/en-us/applemobilitytrends-2020-11-14.csv
##   https://covid19-static.cdn-apple.com/covid19-mobility-data/2021HotfixDev12/v3/en-us/applemobilitytrends-2020-11-15.csv

##https://covid19-static.cdn-apple.com/covid19-mobility-data/2021HotfixDev17/v3/en-us/applemobilitytrends-2020-11-18.csv
##  https://covid19-static.cdn-apple.com/covid19-mobility-data/2021HotfixDev34/v3/en-us/applemobilitytrends-2020-12-01.csv
##https://covid19-static.cdn-apple.com/covid19-mobility-data/2022HotfixDev13/v3/en-us/applemobilitytrends-2020-12-03.csv
## https://covid19-static.cdn-apple.com/covid19-mobility-data/2022HotfixDev27/v3/en-us/applemobilitytrends-2020-12-15.csv
## https://covid19-static.cdn-apple.com/covid19-mobility-data/2023HotfixDev10/v3/en-us/applemobilitytrends-2020-12-17.csv

Yesterday <- Sys.Date()-1

Apple.file <- paste0("https://covid19-static.cdn-apple.com/covid19-mobility-data/2023HotfixDev20/v3/en-us/applemobilitytrends-",Yesterday, ".csv")


#### read the latested Apple mobility report from disk ####
#  Apple_mob_raw <- read.csv(paste("C:\\Rdir\\Mobility\\Apple\\applemobilitytrends-", Yesterday,".csv",sep=""))
Apple_mob_raw <- read.csv(paste(Apple.file ,sep=""))

#### filter only the Netherlands ####
Apple_mob_raw_nl <-Apple_mob_raw %>% filter(region == "Netherlands")

####cleanup the table####
Apple_mob_nl_short <- Apple_mob_raw_nl[ -c(1,2,4,5,6) ]  

###rotate table
Apple_mob_nl_short <- as.data.frame(t(Apple_mob_nl_short))

#### remove first row and set right column names ####
Apple_mob_nl_short <- Apple_mob_nl_short[-1,]
setDT(Apple_mob_nl_short, keep.rownames = TRUE)[]
colnames(Apple_mob_nl_short) <-  c("date","auto","OV","lopen")

#### add the date to a column ####
Apple_mob_nl_short$date <- gsub("[a-zA-Z ]", "", Apple_mob_nl_short$date)
Apple_mob_nl_short$date <- as.Date(Apple_mob_nl_short$date, format = "%Y.%m.%d")

#### right format

Apple_mob_nl_short$auto <- as.numeric(Apple_mob_nl_short$auto)
Apple_mob_nl_short$OV <- as.numeric(Apple_mob_nl_short$OV)
Apple_mob_nl_short$lopen <- as.numeric(Apple_mob_nl_short$lopen)

##### 7day MA #####

Apple_mob_nl_short$MAauto  <- (round(rollmeanr(Apple_mob_nl_short$auto,  7, fill = 0),digits = 1)-100)
Apple_mob_nl_short$MAOV    <- (round(rollmeanr(Apple_mob_nl_short$OV,    7, fill = 0),digits = 1)-100)
Apple_mob_nl_short$MAlopen <- (round(rollmeanr(Apple_mob_nl_short$lopen, 7, fill = 0),digits = 1)-100)

Apple_mob_nl_short <- Apple_mob_nl_short[-1:-6,]



#### gather the table ####

#keycol_am <- "Date"
#valuecol_am <- "type"
#gathercols_am <- c("Auto", "OV","Lopen")

#AppleMobility_gather <- gather(Apple_mob_nl_short, keycol_am, valuecol_am, gathercols_am)

#### set the right valuetype ####
#AppleMobility_gather$keycol_am <- factor(AppleMobility_gather$keycol_am)
#AppleMobility_gather$valuecol_am <- as.numeric(AppleMobility_gather$valuecol_am)


#colnames(AppleMobility_gather) <- c("Datum","Type","valuecol_am")


####   Plot the Apple Mobility data ####




persco.df=data.frame(date=as.Date(c("2020-03-09", "2020-03-12", "2020-03-16", "2020-03-24", "2020-09-18", "2020-09-28", "2020-10-13", "2020-11-03", "2020-10-25", "2020-11-17", "2020-11-27", "2020-12-05", "2020-12-15")), 
                     event=c("Geen handeschudden", "aanvullende maatregelen",  "scholen/horeca dicht", "inteligente lockdown", "kroeg uurtje eerder dicht", "We gaan voor R=0,9","Semi-lockdown", "verzwaring semi-lockdown", "Einde herfstvakantie", "Einde verzwaring", "Black Friday", "Sinterklaas", "lockdown"))


ov.min <-  min(Apple_mob_nl_short$MAOV, na.rm=T)
auto.min <-  min(Apple_mob_nl_short$MAauto , na.rm=T)
lopen.min <-  min(Apple_mob_nl_short$MAlopen, na.rm=T)

ggplot(Apple_mob_nl_short, x=date)+  #aes(Datum, valuecol_am, group=Type, color=Type))+ 
  
  #geom_hline(yintercept = ov.min,    linetype = "dashed", color = "#619cff")+
  #geom_hline(yintercept = auto.min,  linetype = "dashed", color = "#f8766d")+
  #geom_hline(yintercept = lopen.min, linetype = "dashed", color = "#10be45")+
  
  geom_segment(aes(x = as.Date("2020-12-15"), y = -80.5, xend = as.Date(Yesterday), yend = -80.5),linetype = "dashed", color = "#619cff")+
  geom_segment(aes(x = as.Date("2020-12-15"), y = -54.2, xend = as.Date(Yesterday), yend = -54.2),linetype = "dashed", color = "#f8766d")+
  geom_segment(aes(x = as.Date("2020-12-15"), y = -56.1, xend = as.Date(Yesterday), yend = -56.1),linetype = "dashed", color = "#10be45")+
  
  
  geom_hline(yintercept=0) +
  
  geom_vline(data=persco.df, mapping=aes(xintercept=date), color="black", linetype = "dotted") +
  geom_text(data=persco.df, mapping=aes(x=date, y=-98, label=event), size=4, angle=90, vjust=-0.4, hjust=0)+
  
  geom_line(aes(x=date, y = MAlopen, color = "Lopen"), lwd=2) +
  geom_line(aes(x=date, y = MAauto, color = "Auto"), lwd=2) +
    geom_line(aes(x=date, y = MAOV),color = "#F5F5F5", lwd=3) +
  geom_line(aes(x=date, y = MAOV, color = "Openbaar vervoer"), lwd=2) +
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
  scale_y_continuous(expand = c(0,5), limits = c(-100, NA)) +

  scale_x_date(date_breaks = "1 month", 
                 date_labels= format("%b"),
                 limits = as.Date(c("2020-02-20", Yesterday)))+  
    
  labs(title = "Apple Mobility Trends",
       subtitle = paste("7-daags zwevend gemiddele\n","Actueel tot:", Yesterday),
       caption = paste("Source: Apple | Plot: @YorickB | ",Sys.Date()))+
  
  
  theme(legend.position = c(0.2,1),
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.margin = margin(3, 3, 3, 3),
        legend.text = element_text(colour="black", size=20, face="bold"))+
  
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         #legend.position = "none",   # no legend
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=12),  #, angle=45),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
         axis.line = element_line(colour = "#F5F5F5"),
         
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
         ### facet label custom
         strip.text.x = element_text(size = 13, color = "black"),
         strip.background = element_rect(color="black", fill="gray", size=1.5, linetype="solid"))+
  
 # geom_vline(xintercept = as.Date("2020-09-19"), linetype = "dotted") + 
 # geom_vline(xintercept = as.Date("2020-09-29"), linetype = "dotted") + 
 # geom_vline(xintercept = as.Date("2020-10-14"), linetype = "dotted") + 
 # geom_vline(xintercept = as.Date("2020-11-04"), linetype = "dotted") 

  
  ggsave("data/30_Apple_data.png",width=16, height = 9)




#### Apple tweet ####




lastApple <- tail(Apple_mob_nl_short,n=7)


perc_OV   <- as.integer(lastApple$MAOV[7])
perc_walk <- as.integer(lastApple$MAlopen[7])
perc_car  <- as.integer(lastApple$MAauto[7])

perc_OV_week   <- perc_OV   - as.integer(lastApple$MAOV[1])
perc_walk_week <- perc_walk - as.integer(lastApple$MAlopen[1])
perc_car_week  <- perc_car  - as.integer(lastApple$MAauto[1])



emoji_OV <- intToUtf8(0x1F682)
emoji_walk <- intToUtf8(0x1F6B6)
emoji_car <- intToUtf8(0x1F697)

deP <- intToUtf8(0x0025)


tweet.appleM.tweet <- "Apple mobility data:

Laatste datapunt: 
%s

Verandering in routeaanvragen sinds 13 januari 2020:
(verandering t.o.v. een week geleden)
%s   %s%s  (%s%s)  - auto
%s   %s%s  (%s%s)  - lopen
%s   %s%s  (%s%s)  - OV

"

tweet.appleM.tweet <- sprintf(tweet.appleM.tweet,
                              Yesterday,
                              emoji_car, perc_car,deP,perc_car_week,deP,
                              emoji_walk, perc_walk,deP,perc_walk_week,deP,
                              emoji_OV, perc_OV,deP,perc_OV_week,deP)

Encoding(tweet.appleM.tweet) <- "UTF-8"



##  post_tweet(tweet.appleM.tweet,  media = c("data/30_Apple_data.png"))
##  post_tweet(tweet.appleM.tweet,  media = c("data/30_Apple_data.png") , in_reply_to_status_id = get_reply_id()) 

  