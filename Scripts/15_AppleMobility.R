library(tidyverse)
library(rtweet)
library(data.table)

#### authenticate Twitter account ####

source("C:\\Rdir\\Rscripts\\03A_TwitterAuthentication.r")

#### read the latested Apple mobility report from disk ####
Apple_mob_raw <- read.csv(paste("C:\\Rdir\\Mobility\\Apple\\applemobilitytrends-",Sys.Date()-1,".csv",sep=""))

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

#### gather the table ####

keycol_am <- "Date"
valuecol_am <- "type"
gathercols_am <- c("auto", "OV","lopen")

AppleMobility_gather <- gather(Apple_mob_nl_short, keycol_am, valuecol_am, gathercols_am)

#### set the right valuetype ####
AppleMobility_gather$keycol_am <- factor(AppleMobility_gather$keycol_am)
AppleMobility_gather$valuecol_am <- as.numeric(AppleMobility_gather$valuecol_am)


colnames(AppleMobility_gather) <- c("Datum","Type","valuecol_am")


####   Plot the Apple Mobility data ####

ggplot(AppleMobility_gather, aes(Datum, valuecol_am, group=Type, color=Type))+ 
  geom_line(size = 2)+

theme_classic()+
  xlab("")+ 
  ylab("")+
  
  labs(title = "Apple Mobility",
       subtitle = paste("Actueel tot:", Sys.Date()-1),
       caption = paste("Source: Apple | Plot: @YorickB | ",Sys.Date()))+
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
         strip.background = element_rect(color="black", fill="gray", size=1.5, linetype="solid"))


ggsave("data/30_Apple_data.png",width=16, height = 9)




#### Apple tweet ####


AppleMobilityRawNL_sh_t

lastApple <- tail(AppleMobilityRawNL_sh_t,n=1)


perc_OV   <- as.integer(lastApple$OV[1])
perc_walk <- as.integer(lastApple$lopen[1])
perc_car  <- as.integer(lastApple$auto[1])


emoji_OV <- intToUtf8(0x1F682)
emoji_walk <- intToUtf8(0x1F6B6)
emoji_car <- intToUtf8(0x1F697)

L_DataPunt <- Sys.Date()-1
deP <- intToUtf8(0x0025)

tweet.cases.tweet <- "Apple mobility data:

Laatste datapunt: 
%s

Huidig percentage t.o.v vorig jaar:

%s   %s%s
%s   %s%s
%s   %s%s

"

tweet.appleM.tweet <- sprintf(tweet.cases.tweet,
                             L_DataPunt,
                             emoji_car, perc_car,deP,
                             emoji_walk, perc_walk,deP,
                             emoji_OV, perc_OV,deP)

Encoding(tweet.appleM.tweet) <- "UTF-8"



post_tweet(tweet.appleM.tweet,  media = c("data/30_Apple_data.png"))


            