
library(tidyverse)
library(ggrepel)
#library(paletteer)

#import from LCPS website

LCPS_datafeed<-read.csv("https://lcps.nu/wp-content/uploads/covid-19.csv",sep=",")  

LCPS_datafeed$Datum <- as.Date(LCPS_datafeed$Datum ,format="%d-%m-%Y")
LCPS_datafeed$week<-strftime(LCPS_datafeed$Datum,format = "%V")

File_date_5ax <- paste0("data/",format(Sys.time(), "%Y-%m-%d"),"/",format(Sys.time(), "%Y-%m-%d"), "_COVID-19_LCSP.csv")
File_date_5bx <- paste0("LCPS-data/","COVID-19_LCSP_", format(Sys.time(), "%Y-%m-%d"),".csv")

write.csv2(LCPS_datafeed, File_date_5ax, row.names=FALSE) 
write.csv2(LCPS_datafeed, File_date_5bx, row.names=FALSE) 

#import from storage

LCPS_datafeed_old<-read.csv("C:\\Rdir\\LCPS-data\\tm11_01.csv",sep=";")  
LCPS_datafeed_old$date <- as.Date(LCPS_datafeed_old$date ,format="%Y-%m-%d")

#### copy to tempfiles ####

lcps_working_1 <- LCPS_datafeed
lcps_working_old <- LCPS_datafeed_old
colnames(lcps_working_1) <- c("date", "IC_covid_nl", "IC_neg" ,"clinic_nl","IC_opname", "clinic_opname", "week")

##### prepare for comibiantion #####

today<-strftime(Sys.Date(),format = "%Y-%m-%d")
lcps_working_1 <- lcps_working_1[lcps_working_1$date>"2020-11-01"&lcps_working_1$date<=today,]
lcps_working_2 <- lcps_working_old[lcps_working_old$date>"2020-02-26"&lcps_working_old$date<="2020-11-01",]



##### data for IC graph  ####


lcps_working_1a  <- lcps_working_1[,c("date","IC_covid_nl","IC_neg")]
lcps_working_2a  <- lcps_working_2[,c("date","IC_covid_nl","IC_neg")]

bothdfs22 <- rbind(lcps_working_2a, lcps_working_1a)

#bothdfs22 <- bothdfs22 %>% filter(bothdfs22$date >"2020-03-07")

colnames(bothdfs22) <- c("date", "B_IC_covid_nl", "A_IC_neg")
#lcps_working_2 <- lcps_working_1_short[,c("date","IC_covid_nl","IC_neg")]
lcps_working_2_long<- gather(bothdfs22, "B_IC_covid_nl","A_IC_neg", -date)
colnames(lcps_working_2_long) <- c("date", "type", "number")



#### data for Hospital graph  ####

lcps_working_1b  <- lcps_working_1[,c("date","IC_covid_nl","clinic_nl")]
lcps_working_2b  <- lcps_working_2[,c("date","IC_covid_nl","clinic_nl")]

bothdfs11 <- rbind(lcps_working_2b, lcps_working_1b)

colnames(bothdfs11) <- c("date", "IC_covid_nl", "clinic_nl")
#lcps_working_tot_c_1 <- lcps_working_1_short[,c("date","IC_covid_nl","clinic_nl")]
lcps_working_tot_c_1_long <- gather(bothdfs11, "IC_covid_nl","clinic_nl", -date)
colnames(lcps_working_tot_c_1_long) <- c("date", "type", "number")



###  IC plot

bothdfs22 <-bothdfs22[order(bothdfs22$date),]
lcps.sh.2 <- tail(bothdfs22,n=2)

hosp.total.x   <- as.integer(lcps.sh.2$B_IC_covid_nl [1]+lcps.sh.2$A_IC_neg[1])  #yesterday
hosp.total.y   <- as.integer(lcps.sh.2$B_IC_covid_nl [2]+lcps.sh.2$A_IC_neg[2])  #today
hosp.total.z   <- hosp.total.y - hosp.total.x
hosp.total.y <- format(hosp.total.y, big.mark="." ,decimal.mark=",")
hosp_title.a <- paste0("Aantal patienten met COVID-19 nu in het ziekenhuis: ", hosp.total.y, "  (", hosp.total.z , ")")

hosp.total.x1   <- as.integer(lcps.sh.2$A_IC_neg[1])  #yesterday
hosp.total.y1   <- as.integer(lcps.sh.2$A_IC_neg[2])  #today
hosp.total.z1   <- hosp.total.y1 - hosp.total.x1
hosp.total.y1  <- format( hosp.total.y1, big.mark="." ,decimal.mark=",")
hosp_clin.a <- paste0("Kliniek: ",hosp.total.y1, "  (", hosp.total.z1 , ")")

hosp.total.x2   <- as.integer(lcps.sh.2$B_IC_covid_nl[1])  #yesterday
hosp.total.y2   <- as.integer(lcps.sh.2$B_IC_covid_nl[2])  #today
hosp.total.z2   <- hosp.total.y2 - hosp.total.x2
hosp.total.y2  <- format( hosp.total.y2, big.mark="." ,decimal.mark=",")
hosp_IC.a <- paste0("IC:          ",hosp.total.y2, "   (", hosp.total.z2 , ")")




ggplot(data = lcps_working_2_long, mapping = aes(x = date, y = number, color = type, fill = type))+
      geom_bar(stat='identity')+
        
      scale_x_date(name="")+
      ylab("")+
 
      scale_fill_manual  (values=c("#C5E0B4","#4472C4"), labels=c(hosp_clin.a, hosp_IC.a))+
       scale_color_manual(values=c("#767171","#3B3838"), labels=c(hosp_clin.a, hosp_IC.a))+
    
    labs(title=hosp_title.a, # subtitle=" ",
         caption = paste("Source: LCPS & NICE | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  
  theme(legend.position = c(0.5, 0.9),
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
          axis.line = element_line(colour = "#DAE3F3"),
          panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))

ggsave("data/17_IC_only.png",width=16, height = 9)


### total COVID-19 plot


bothdfs11 <-bothdfs11[order(bothdfs11$date),]
lcps.sh.1 <- tail(bothdfs11,n=2)

hosp.total.a   <- as.integer(lcps.sh.1$IC_covid_nl [1]+lcps.sh.1$clinic_nl[1])  #yesterday
hosp.total.b   <- as.integer(lcps.sh.1$IC_covid_nl [2]+lcps.sh.1$clinic_nl[2])  #today
hosp.total.c   <- hosp.total.b - hosp.total.a
hosp.total.b <- format(hosp.total.b, big.mark="." ,decimal.mark=",")
hosp_title <- paste0("Aantal patienten met COVID-19 nu in het ziekenhuis: ", hosp.total.b, "  (", hosp.total.c , ")")

hosp.total.a1   <- as.integer(lcps.sh.1$clinic_nl[1])  #yesterday
hosp.total.b1   <- as.integer(lcps.sh.1$clinic_nl[2])  #today
hosp.total.c1   <- hosp.total.b1 - hosp.total.a1
hosp.total.b1  <- format( hosp.total.b1, big.mark="." ,decimal.mark=",")
hosp_clin <- paste0("Kliniek: ",hosp.total.b1, "  (", hosp.total.c1 , ")")

hosp.total.a2   <- as.integer(lcps.sh.1$IC_covid_nl[1])  #yesterday
hosp.total.b2   <- as.integer(lcps.sh.1$IC_covid_nl[2])  #today
hosp.total.c2   <- hosp.total.b2 - hosp.total.a2
hosp.total.b2  <- format( hosp.total.b2, big.mark="." ,decimal.mark=",")
hosp_IC <- paste0("IC:            ",hosp.total.b2, "   (", hosp.total.c2 , ")")



ggplot(data = lcps_working_tot_c_1_long, mapping = aes(x = date, y = number, color = type, fill = type))+
  geom_bar(stat='identity')+
  
  scale_x_date(name="")+
  ylab("")+
  
  scale_fill_manual (values=c("#F4B183","#4472C4"), labels=c(hosp_clin, hosp_IC))+
  scale_color_manual(values=c("#843C0C","#3B3838"), labels=c(hosp_clin, hosp_IC))+
  
  labs(title = hosp_title, # subtitle=" ",
       caption = paste("Source: LCPS & NICE | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  
  theme(legend.position = c(0.5, 0.9),
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
          axis.line = element_line(colour = "#DAE3F3"),
          panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))

ggsave("data/16_IC_hosp.png",width=16, height = 9)



#  annotation_custom(grob)+facet_wrap(~cyl, scales="free")




ggplot(data = lcps_working_2_long, mapping = aes(x = date, y = number, fill = type)) + 
  geom_stream(bw=0.08)+
  
  scale_x_date(name="")+
  
  scale_y_continuous(name=" ", breaks=c(-800,-400,0,400,800),
                     labels=c("800", "400", "0", "400","800"))+
  
  # scale_fill_manual(values=c("#4472C4","#C5E0B4"), name=" test ",labels=c("IC - covid", "IC - non-covid"))+
  scale_fill_manual(values=c("#C5E0B4","#4472C4"), name=" test ",labels=c("IC - non-covid", "IC - covid"))+
  
  
  labs(title="(non-) COVID-19 patienten op de IC",
       # subtitle=" ",
       caption = paste("Source: LCPS & NICE | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  
  theme(legend.position = c(0.5, 0.9),
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

ggsave("data/17_IC_only-2.png",width=16, height = 9)





                    