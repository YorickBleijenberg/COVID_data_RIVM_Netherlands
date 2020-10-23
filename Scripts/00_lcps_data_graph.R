
library(tidyverse)
library(ggrepel)
#library(paletteer)

#import from LCPS website

LCPS_datafeed<-read.csv("https://lcps.nu/wp-content/uploads/covid-19.csv",sep=",")  

(LCPS_datafeed)

LCPS_datafeed$Datum <- as.Date(LCPS_datafeed$Datum ,format="%d-%m-%Y")
LCPS_datafeed$week<-strftime(LCPS_datafeed$Datum,format = "%V")

File_date_5a <- paste0("data/",format(Sys.time(), "%Y-%m-%d"),"/",format(Sys.time(), "%Y-%m-%d"), "_COVID-19_LCSP.csv")
File_date_5b <- paste0("LCPS-data/","COVID-19_LCSP_", format(Sys.time(), "%Y-%m-%d"),".csv")

write.csv2(LCPS_datafeed, File_date_5a, row.names=FALSE) 
write.csv2(LCPS_datafeed, File_date_5b, row.names=FALSE) 

#import from storage

LCPS_datafeed_old<-read.csv("C:\\Rdir\\LCPS-data\\tm10_07.csv",sep=";")  
LCPS_datafeed_old$date <- as.Date(LCPS_datafeed_old$date ,format="%Y-%m-%d")

(LCPS_datafeed_old)


lcps_working_1 <- LCPS_datafeed
lcps_working_old <- LCPS_datafeed_old
colnames(lcps_working_1) <- c("date", "IC_covid_nl", "IC_neg" ,"clinic_nl", "week")

today<-strftime(Sys.Date(),format = "%Y-%m-%d")
lcps_working_1_short <- lcps_working_1[lcps_working_1$date>"2020-07-10"&lcps_working_1$date<=today,]
lcps_working_old_short <- lcps_working_old[lcps_working_old$date>"2020-02-26"&lcps_working_old$date<="2020-07-10",]




lcps_working_1_short_2bind    <- lcps_working_1_short[,c("date","IC_covid_nl","IC_neg")]
lcps_working_old_short_2bind  <- lcps_working_old_short[,c("date","IC_covid_nl","IC_neg")]

bothdfs22 <- rbind(lcps_working_old_short_2bind, lcps_working_1_short_2bind)


lcps_working_1_short_2bind    <- lcps_working_1_short[,c("date","IC_covid_nl","clinic_nl")]
lcps_working_old_short_2bind  <- lcps_working_old_short[,c("date","IC_covid_nl","clinic_nl")]

bothdfs11 <- rbind(lcps_working_old_short_2bind, lcps_working_1_short_2bind)






#lcps_working_2 <- lcps_working_1_short[,c("date","IC_covid_nl","IC_neg")]
lcps_working_2_long<- gather(bothdfs22, "IC_covid_nl","IC_neg", -date)
colnames(lcps_working_2_long) <- c("date", "type", "number")



#lcps_working_tot_c_1 <- lcps_working_1_short[,c("date","IC_covid_nl","clinic_nl")]
lcps_working_tot_c_1_long <- gather(bothdfs11, "IC_covid_nl","clinic_nl", -date)
colnames(lcps_working_tot_c_1_long) <- c("date", "type", "number")









###  IC plot

ggplot(data = lcps_working_2_long, mapping = aes(x = date, y = number, color = type)) + 
  geom_bar(stat='identity')+
  geom_text(x=3, y=30, label="Test5")
#  annotation_custom(grob)+facet_wrap(~cyl, scales="free")



ggplot(data = lcps_working_2_long, mapping = aes(x = date, y = number, fill = type)) + 
  geom_stream(bw=0.08)+
  
  scale_x_date(name="")+
  
  scale_y_continuous(name=" ", breaks=c(-800,-400,0,400,800),
                     labels=c("800", "400", "0", "400","800"))+
  
  scale_fill_manual(values=c("#4472C4","#C5E0B4"), name=" test ",labels=c("IC covid", "non-covid"))+
  
  
  
  
  labs(title="COVID-19 patienten in het ziekenhuis",
       # subtitle=" ",
       caption = paste("Source: LCPS & NICE | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  
  theme(#legend.position = c(0.8, 0.9),
    legend.background = element_rect(fill="#DAE3F3",size=0.8,linetype="solid"), #,colour ="black"),
    legend.title = element_blank(),
    legend.text = element_text(colour="black", size=20, face="bold"))+
  
  theme(  plot.background = element_rect(fill = "#DAE3F3"), #background color/size (border color and size)
          plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
          panel.background = element_rect(fill = "#DAE3F3", colour = "#DAE3F3"),
          axis.text = element_text(size=14,color = "black",face = "bold"),
          axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
          axis.ticks = element_line(colour = "#DAE3F3", size = 1, linetype = "solid"),
          axis.ticks.length = unit(0.5, "cm"),
          axis.line = element_line(colour = "#DAE3F3"))





ggsave("data/17_IC_only.png",width=16, height = 9)




### total COVID-19 plot

ggplot(data = lcps_working_tot_c_1_long, mapping = aes(x = date, y = number, color = type)) + 
  geom_bar(stat='identity')+
  geom_text(x=3, y=30, label="Test5")


ggplot(data = lcps_working_tot_c_1_long, mapping = aes(x = date, y = number, fill = type)) + 
  geom_stream(bw=0.08)+
  
  scale_x_date(name="")+
  scale_y_continuous(name="COVID-19 patienten in het ziekenhuis", breaks=c(-2000,-1000,0,1000,2000),
                     labels=c("2.000", "1.000", "0", "1.000","2.000"))+
  
  scale_fill_manual(values=c("#F4B183","#4472C4"), name=" test ",labels=c("kliniek", "IC"))+
  
  labs(title="COVID-19 patienten in het ziekenhuis",
       # subtitle=" ",
       caption = paste("Source: LCPS & NICE | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  
  theme(#legend.position = c(0.8, 0.9),
    legend.background = element_rect(fill="#DAE3F3",size=0.8,linetype="solid"), #,colour ="black"),
    legend.title = element_blank(),
    legend.text = element_text(colour="black", size=20, face="bold"))+
  
  theme(  plot.background = element_rect(fill = "#DAE3F3"), #background color/size (border color and size)
          plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
          panel.background = element_rect(fill = "#DAE3F3", colour = "#DAE3F3"),
          axis.text = element_text(size=14,color = "black",face = "bold"),
          axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
          axis.ticks = element_line(colour = "#DAE3F3", size = 1, linetype = "solid"),
          axis.ticks.length = unit(0.5, "cm"),
          axis.line = element_line(colour = "#DAE3F3")
  )

ggsave("data/16_IC_hosp.png",width=16, height = 9)



























                    