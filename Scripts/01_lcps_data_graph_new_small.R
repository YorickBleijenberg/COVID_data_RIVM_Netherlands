

library(tidyverse)
#library(ggrepel)
library(rtweet)
#library(data.table)
#library(paletteer)
#library(scales)


today <- Sys.Date()


get_reply_id <- function(rel_increase) {
  my_timeline <- get_timeline(rtweet:::home_user()) ## Pull my own tweets
  reply_id <- my_timeline$status_id[1] ## Status ID for reply
  return(reply_id)
}

### import local

###  LCPS_datafeed<-read.csv("C:\\Rdir\\data\\plots\\covid-19.csv",sep=",")  

#import from LCPS website

LCPS_datafeed<-read.csv("https://lcps.nu/wp-content/uploads/covid-19-datafeed.csv",sep=",")  
#LCPS_datafeed<-read.csv("C:\\Rdir\\data\\2021-11-28\\2021-11-28_COVID-19_LCSP2.csv", sep=";")
LCPS_datafeed$Datum <- as.Date(LCPS_datafeed$Datum ,format="%d-%m-%Y")
#LCPS_datafeed$Datum <- as.Date(LCPS_datafeed$Datum ,format="%Y-%m-%d")
#LCPS_datafeed$week<-strftime(LCPS_datafeed$Datum,format = "%V")



File_date_5ax <- paste0("data/",format(Sys.time(), "%Y-%m-%d"),"/",format(Sys.time(), "%Y-%m-%d"), "_COVID-19_LCSP2.csv")
File_date_5bx <- paste0("LCPS-data/","COVID-19_LCSP_", format(Sys.time(), "%Y-%m-%d"),"2.csv")

write.csv2(LCPS_datafeed, File_date_5ax, row.names=FALSE) 
write.csv2(LCPS_datafeed, File_date_5bx, row.names=FALSE) 

#import from storage

LCPS_datafeed_old<-read.csv("C:\\Rdir\\LCPS-data\\tm11_01.csv",sep=";")  
LCPS_datafeed_old$date <- as.Date(LCPS_datafeed_old$date ,format="%Y-%m-%d")


#####  remove international ####
 LCPS_datafeed <- LCPS_datafeed[,-c(3)]


LCPS_datafeed[is.na(LCPS_datafeed)] <- 0

#### copy to tempfiles ####

#Aant het typen voor RTL Nieuws ## easter egg

lcps_working_1 <- LCPS_datafeed
lcps_working_old <- LCPS_datafeed_old
colnames(lcps_working_1) <- c("date", "IC_covid_nl", "IC_neg" ,"clinic_nl","IC_opname", "clinic_opname")

##### prepare for combination #####

today<-strftime(Sys.Date(),format = "%Y-%m-%d")

lcps_working_1 <- lcps_working_1[lcps_working_1$date>"2020-10-17"&lcps_working_1$date<=today,]
lcps_working_old <- lcps_working_old[lcps_working_old$date>"2020-02-26"&lcps_working_old$date<="2020-10-17",]





##### data for IC graph  ####

beds_non_covid  <- first(LCPS_datafeed$IC_Bedden_Non_COVID)
beds_covid      <- first(LCPS_datafeed$IC_Bedden_COVID)


lcps_working_1a  <- lcps_working_1[,c("date","IC_covid_nl","IC_neg")]
lcps_working_2a  <- lcps_working_old[,c("date","IC_covid_nl","IC_neg")]

lcps.both.df <- rbind(lcps_working_2a, lcps_working_1a)

colnames(lcps.both.df) <- c("date", "B_IC_covid_nl", "A_IC_neg")

IC_occupation <- lcps.both.df

lcps_working_2_long<- gather(lcps.both.df, "B_IC_covid_nl","A_IC_neg", -date)
colnames(lcps_working_2_long) <- c("date", "type", "number")



####  IC plot ####

lcps.both.df <-lcps.both.df[order(lcps.both.df$date),]
lcps.sh.2 <- tail(lcps.both.df,n=2)

hosp.total.x   <- as.integer(lcps.sh.2$B_IC_covid_nl [1]+lcps.sh.2$A_IC_neg[1])  #yesterday
hosp.total.y   <- as.integer(lcps.sh.2$B_IC_covid_nl [2]+lcps.sh.2$A_IC_neg[2])  #today
hosp.total.boss <- hosp.total.y + 188
hosp.total.z   <- hosp.total.y - hosp.total.x
hosp.total.y <- format(hosp.total.y, big.mark="." ,decimal.mark=",")
hosp_title.a <- paste0("Aantal patienten op de IC: ", hosp.total.y, "  (", hosp.total.z , ")")

hosp.total.boss <- format(hosp.total.boss, big.mark="." ,decimal.mark=",")
hosp_title.boss <- paste0("Benodigde IC capaciteit: ", hosp.total.boss, " bedden")



hosp.total.x1   <- as.integer(lcps.sh.2$A_IC_neg[1])  #yesterday
hosp.total.y1   <- as.integer(lcps.sh.2$A_IC_neg[2])  #today
hosp.total.y1.level  <- hosp.total.y1
hosp.total.z1   <- hosp.total.y1 - hosp.total.x1
hosp.total.y1  <- format( hosp.total.y1, big.mark="." ,decimal.mark=",")
hosp_clin.a <- paste0("non-covid:   ",hosp.total.y1, "  (", hosp.total.z1 , ")")

hosp.total.x2   <- as.integer(lcps.sh.2$B_IC_covid_nl[1])  #yesterday
hosp.total.y2   <- as.integer(lcps.sh.2$B_IC_covid_nl[2])  #today
hosp.total.z2   <- hosp.total.y2 - hosp.total.x2
hosp.total.y2  <- format( hosp.total.y2, big.mark="." ,decimal.mark=",")
hosp_IC.a <- paste0("covid:          ",hosp.total.y2, "   (", hosp.total.z2 , ")")


####  Set date ####

lcps_working_2_long <- lcps_working_2_long[lcps_working_2_long$date>"2020-03-06",]




ggplot(data = lcps_working_2_long, mapping = aes(x = date, y = number, color = type, fill = type))+

        geom_bar(stat='identity')+
        
  scale_x_date(name="")+
      ylab("")+
  scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","),
                      breaks=c(250,500,750,1000,1250,1500),
                      sec.axis = dup_axis()
                      )+

  
  ####  coord_cartesian(expand = FALSE)+
  
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
          panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))+

ggsave("data/plots/16b_IC_only.png",width=16, height = 9)





#### total IC plot reverse ####


#ggplot(data = lcps_working_tot_c_1_long, mapping = aes(x = date, y = number, color = factor(type, levels=c("IC_covid_nl", "clinic_nl"))))+  #, fill = type))+
  
  ggplot(data = lcps_working_2_long, mapping = aes(x = date, y = number, color = factor(type, levels=c("B_IC_covid_nl", "A_IC_neg")),fill = factor(type, levels=c("B_IC_covid_nl", "A_IC_neg"))))+
  
  geom_bar(stat='identity')+
  
  scale_x_date(name="")+
  ylab("")+
  scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","),
                      breaks=c(250,500,750,1000,1250,1500),
                      sec.axis = dup_axis()
  )+
#  scale_fill_manual  (values=c("#C5E0B4","#4472C4"), labels=c(hosp_clin.a, hosp_IC.a))+
#  scale_color_manual(values=c("#767171","#3B3838"), labels=c(hosp_clin.a, hosp_IC.a))+
  
  scale_fill_manual  (values=c("#4472C4","#C5E0B4"), labels=c(hosp_IC.a, hosp_clin.a))+
  scale_color_manual(values=c("#3B3838","#767171"), labels=c(hosp_IC.a, hosp_clin.a))+
  
  labs(title= "Afschaling reguliere zorg", 
       subtitle="Minimum IC eerste golf: 345 bedden non-covid",
       caption = paste("Source: LCPS & NICE | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  
  theme(legend.position = c(0.5, 0.9),
        legend.background = element_rect(fill="#DAE3F3",size=0.8,linetype="solid"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=20, face="bold"))+
  
  theme(  plot.background = element_rect(fill = "#DAE3F3"), 
          plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
          plot.subtitle = element_text(hjust = 0.5,size = 10),
          panel.background = element_rect(fill = "#DAE3F3", colour = "#DAE3F3"),
          axis.text = element_text(size=14,color = "black",face = "bold"),
          axis.text.y = element_text(face="bold", color="black", size=14),
          axis.ticks = element_line(colour = "#DAE3F3", size = 1, linetype = "solid"),
          axis.ticks.length = unit(0.5, "cm"),
          axis.line = element_line(colour = "#DAE3F3"),
          panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))+
  
  geom_segment(aes(x = as.Date("2020-03-10"), y = hosp.total.y1.level, xend = as.Date(today), yend = hosp.total.y1.level),linetype = "dashed", color = "red")+
  
  
  ggsave("data/plots/16b_IC_only_reverse.png",width=16, height = 9)






ggplot(data = lcps_working_2_long, mapping = aes(x = date, y = number, color = type, fill = type))+
  
  geom_bar(stat='identity', position=position_fill(), width=1)+
  
  scale_x_date(name="")+
  ylab("")+
 scale_y_continuous( labels = scales::percent,
                     sec.axis = dup_axis())+
                    #  breaks=c(250,500,750,1000,1250,1500),
                    # sec.axis = dup_axis()
 # )+
  
  scale_fill_manual  (values=c("#C5E0B4","#4472C4"), labels=c("non-covid", "covid"))+
  scale_color_manual(values=c("#767171","#3B3838"), labels=c("non-covid", "covid"))+
  
  labs(title = "verhouding covid / non-covid op de IC", # subtitle=" ",
       caption = paste("Source: LCPS & NICE | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  
  theme(legend.position = c(0.5, 0.9),
        legend.background = element_rect(fill="#DAE3F3",size=0.8,linetype="solid",colour ="black"),
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
          panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))+
  
  ggsave("data/plots/16b_IC_only_fill.png",width=16, height = 9)


IC_occupation2  <- IC_occupation
IC_occupation2$boss <-188


keycol <- "date"
valuecol <- "type"
gathercols <- c("B_IC_covid_nl","A_IC_neg","boss")


lcps_working_3_long <- gather(IC_occupation2, keycol, valuecol, gathercols)



colnames(lcps_working_3_long) <- c("date", "type", "number")



#### zoom ####
ggplot(data = lcps_working_3_long,
       mapping = aes(x = date, y = number, color = factor(type, levels=c("boss","A_IC_neg", "B_IC_covid_nl")), fill = factor(type, levels=c("boss","A_IC_neg", "B_IC_covid_nl"))))+
  
  annotate("rect", xmin = as.Date("2021-08-12"), xmax =as.Date("2021-12-31"), ymin =1150, ymax = 1350, color = "black",fill = "black", alpha = 0.3)+ 
  annotate("rect", xmin = as.Date("2021-08-12"), xmax =as.Date("2021-12-31"), ymin =950, ymax = 1150, color = "black",fill = "red", alpha = 0.5)+ 
  annotate("rect", xmin = as.Date("2021-08-12"), xmax =as.Date("2021-12-31"), ymin =400, ymax = 950, color = "black",fill = "blue", alpha = 0.3)+ 
  annotate("rect", xmin = as.Date("2021-08-12"), xmax =as.Date("2021-12-31"), ymin =200, ymax = 400, color = "black",fill = "red", alpha = 0.3)+ 
  annotate("rect", xmin = as.Date("2021-08-12"), xmax =as.Date("2021-12-31"), ymin =100, ymax = 200, color = "black",fill = "yellow", alpha = 0.3)+ 
  annotate("rect", xmin = as.Date("2021-08-12"), xmax =as.Date("2021-12-31"), ymin =0, ymax = 100, color = "black",fill = "blue", alpha = 0.3)+ 
 
  
  geom_text( aes( x=as.Date("2021-12-15"), y=1330, label="1.350 - Hugo doel"),
             color="black", size=4 , angle=0, fontface="bold")+ 
  
  geom_text( aes( x=as.Date("2021-12-15"), y=1130, label="1.150 - IC max (piek) "),
             color="black", size=4 , angle=0, fontface="bold")+ 
  
  geom_text( aes( x=as.Date("2021-12-15"), y=930, label="950 - IC max (langdurig)"),
             color="black", size=4 , angle=0, fontface="bold")+ 
  
  geom_text( aes( x=as.Date("2021-12-15"), y=380, label="400 - max COVID-19 (zonder griep)"),
             color="black", size=4 , angle=0, fontface="bold")+
  
  geom_text( aes( x=as.Date("2021-12-15"), y=180, label="200 - max COVID-19 (met griep)"),
             color="black", size=4 , angle=0, fontface="bold")+
  
  geom_text( aes( x=as.Date("2021-12-15"), y=50, label="Gommers goed zorg doel:\n max 100 covid op IC"),
             color="black", size=4 , angle=0, fontface="bold")+
  
  geom_bar(stat='identity')+
  
  scale_x_date(name="",
               date_labels= format("%d %b"),
               limits = as.Date(c("2021-08-01", NA)))+
  ylab("")+
  scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","),
                      breaks=c(250,500,750,950,1150,1350,1500),
                      sec.axis = dup_axis(),
                      limits = c(0,1350)
  )+
  
  scale_fill_manual(values=c("darkgray", "#C5E0B4","#4472C4"), labels=c("BOSS bedden:  188", hosp_clin.a, hosp_IC.a))+
  scale_color_manual(values=c("black"  , "#767171","#3B3838"),  labels=c("BOSS bedden:  188", hosp_clin.a, hosp_IC.a))+
  
  labs(title=hosp_title.boss,
       caption = paste("Source: LCPS | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  
  theme(legend.position = c(0.5, 0.9),
        legend.background = element_rect(fill="#DAE3F3",size=0.8,linetype="solid"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=20, face="bold"))+
  
  theme(  plot.background = element_rect(fill = "#DAE3F3"),
          plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
          panel.background = element_rect(fill = "#DAE3F3", colour = "#DAE3F3"),
          axis.text = element_text(size=14,color = "black",face = "bold"),
          axis.text.y = element_text(face="bold", color="black", size=14),
          axis.ticks = element_line(colour = "#DAE3F3", size = 1, linetype = "solid"),
          axis.ticks.length = unit(0.5, "cm"),
          axis.line = element_line(colour = "#DAE3F3"),
          panel.grid.major.y = element_line(colour= "gray", linetype = "dashed")) 
  
 # ggsave("data/plots/16b_IC_only_zoom.png",width=16, height = 9)




####  Flu  #####


lcps.and.flu <- lcps.both.df

lcps.and.flu$Flu <- 0




key <- "Datum"
value <- "type"
gathercols <- c("B_IC_covid_nl","A_IC_neg", "Flu")

lcps_flu_long <- gather(lcps.and.flu, key, value, all_of(gathercols))
colnames(lcps_flu_long) <- c("date", "type", "number")



flu.label <- paste0("Flu:              No data")


#### zoom ####
ggplot(data = lcps_flu_long, mapping = aes(x = date, y = number, color = type, fill = type))+
 
  geom_bar(stat='identity')+
  
  scale_x_date(name="")+
  
  ylab("")+
  scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","),
                      breaks=c(250,500,750,1000,1250,1500),
                      sec.axis = dup_axis(),
                      limits = c(0,NA)
  )+
  
  scale_fill_manual(values=c("#C5E0B4","#4472C4", "red"), labels=c(hosp_clin.a, hosp_IC.a, flu.label))+
  scale_color_manual(values=c("#767171","#3B3838", "red"), labels=c(hosp_clin.a, hosp_IC.a, flu.label))+
  
  labs(title=hosp_title.a,
       caption = paste("Source: LCPS & NICE | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  
  theme(legend.position = c(0.5, 0.9),
        legend.background = element_rect(fill="#DAE3F3",size=0.8,linetype="solid"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=20, face="bold"))+
  
  theme(  plot.background = element_rect(fill = "#DAE3F3"),
          plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
          panel.background = element_rect(fill = "#DAE3F3", colour = "#DAE3F3"),
          axis.text = element_text(size=14,color = "black",face = "bold"),
          axis.text.y = element_text(face="bold", color="black", size=14),
          axis.ticks = element_line(colour = "#DAE3F3", size = 1, linetype = "solid"),
          axis.ticks.length = unit(0.5, "cm"),
          axis.line = element_line(colour = "#DAE3F3"),
          panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))#+
  
 # ggsave("data/plots/16b_IC_only_flu.png",width=16, height = 9)




#### data for Hospital graph  ####

lcps_working_1a  <- lcps_working_1[,c("date","IC_covid_nl","clinic_nl")]
lcps_working_2a  <- lcps_working_old[,c("date","IC_covid_nl","clinic_nl")]

lcps.both.df <- rbind(lcps_working_2a, lcps_working_1a)

colnames(lcps.both.df) <- c("date", "IC_covid_nl", "clinic_nl")

lcps_working_tot_c_1_long <- gather(lcps.both.df, "IC_covid_nl","clinic_nl", -date)
colnames(lcps_working_tot_c_1_long) <- c("date", "type", "number")


#### total COVID-19 plot ####


lcps.both.df <-lcps.both.df[order(lcps.both.df$date),]
lcps.sh.1 <- tail(lcps.both.df,n=2)

hosp.total.a   <- as.integer(lcps.sh.1$IC_covid_nl [1]+lcps.sh.1$clinic_nl[1])  #yesterday
hosp.total.b   <- as.integer(lcps.sh.1$IC_covid_nl [2]+lcps.sh.1$clinic_nl[2])  #today
hosp.total.now <- hosp.total.b
hosp.total.c   <- hosp.total.b - hosp.total.a
hosp.total.b <- format(hosp.total.b, big.mark="." ,decimal.mark=",")
hosp_title <- paste0("Aantal patienten met COVID-19 nu in het ziekenhuis: ", hosp.total.b, "  (", hosp.total.c , ")")

hosp.total.a1   <- as.integer(lcps.sh.1$clinic_nl[1])  #yesterday
hosp.total.b1   <- as.integer(lcps.sh.1$clinic_nl[2])  #today
hosp.total.c1   <- hosp.total.b1 - hosp.total.a1
hosp.total.b1  <- format( hosp.total.b1, big.mark="." ,decimal.mark=",")
hosp_clin <- paste0("Kliniek:  ",hosp.total.b1, "  (", hosp.total.c1 , ")")

hosp.IC.a2   <- as.integer(lcps.sh.1$IC_covid_nl[1])  #yesterday
hosp.IC.b2   <- as.integer(lcps.sh.1$IC_covid_nl[2])  #today
hosp.IC.c2   <- hosp.IC.b2 - hosp.IC.a2
hosp.IC.b2  <- format( hosp.IC.b2, big.mark="." ,decimal.mark=",")
hosp_IC <- paste0("IC:          ",hosp.IC.b2, "  (", hosp.IC.c2 , ")")


#### total hospital plot ####


ggplot(data = lcps_working_tot_c_1_long, mapping = aes(x = date, y = number, color = type, fill = type))+
  geom_bar(stat='identity')+
  
  scale_x_date(name="")+
  ylab("")+
  scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","))+
  scale_fill_manual (values=c("#F4B183","#4472C4"), labels=c(hosp_clin, hosp_IC))+
  scale_color_manual(values=c("#843C0C","#3B3838"), labels=c(hosp_clin, hosp_IC))+
  
  labs(title = hosp_title, # subtitle=" ",
       caption = paste("Source: LCPS | Plot: @YorickB | ",Sys.Date()))+
  
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
        panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))+

geom_segment(aes(x = as.Date("2020-09-01"), y = hosp.total.now, xend = as.Date(today), yend = hosp.total.now),linetype = "dotted", color = "black")+


ggsave("data/plots/16a_IC_hosp.png",width=16, height = 9)






#### data for NEW hosp graph  ####

lcps_working_1a  <- lcps_working_1[,c("date","IC_opname","clinic_opname")]
lcps_working_2a  <- lcps_working_old[,c("date","IC_opname","clinic_opname")]

lcps.both.df <- rbind(lcps_working_2a, lcps_working_1a)



####  new hosp. plot ####

lcps.both.df <-lcps.both.df[order(lcps.both.df$date),]
lcps.sh.3 <- tail(lcps.both.df,n=2)

hosp.new.a   <- as.integer(lcps.sh.3$IC_opname [1]+lcps.sh.3$clinic_opname[1])  #yesterday
hosp.new.b   <- as.integer(lcps.sh.3$IC_opname [2]+lcps.sh.3$clinic_opname[2])  #today
hosp.new.c   <- hosp.new.b - hosp.new.a
hosp.new.b <- format(hosp.new.b, big.mark="." ,decimal.mark=",")
hosp_new_title <- paste0("Aantal patienten nieuw opgenomen in het ziekenhuis: ", hosp.new.b, "  (", hosp.new.c , ")")

hosp.new.a1   <- as.integer(lcps.sh.3$clinic_opname[1])  #yesterday
hosp.new.b1   <- as.integer(lcps.sh.3$clinic_opname[2])  #today
hosp.new.c1   <- hosp.new.b1 - hosp.new.a1
hosp.new.b1  <- format( hosp.new.b1, big.mark="." ,decimal.mark=",")
hosp_New_clin <- paste0("Kliniek: ",hosp.new.b1, "  (", hosp.new.c1 , ")")

hosp.new.a2   <- as.integer(lcps.sh.3$IC_opname[1])  #yesterday
hosp.new.b2   <- as.integer(lcps.sh.3$IC_opname[2])  #today
hosp.new.c2   <- hosp.new.b2 - hosp.new.a2
hosp.new.b2  <- format( hosp.new.b2, big.mark="." ,decimal.mark=",")
hosp_new_IC <- paste0("IC:            ",hosp.new.b2, "   (", hosp.new.c2 , ")")


####  Set date ####

lcps.both.df <- gather(lcps.both.df, "IC_opname","clinic_opname", -date)
colnames(lcps.both.df) <- c("date", "type", "number")

lcps.both.df <- lcps.both.df[lcps.both.df$date>"2020-10-13",]


ggplot(data = lcps.both.df, mapping = aes(x = date, y = number, color = type, fill = type))+

  geom_bar(stat='identity', position = "dodge")+
  geom_hline(yintercept=3)+
  geom_hline(yintercept=10)+
  scale_x_date(name="")+
  ylab("")+
  
  scale_fill_manual  (values=c("#F4B183","#4472C4"), labels=c(hosp_New_clin, hosp_new_IC))+
  scale_color_manual(values=c("#767171","#3B3838"), labels=c(hosp_New_clin, hosp_new_IC))+
  
  labs(title=hosp_new_title, 
       subtitle="lijnen: 10 & 3 IC opnames per dag",
       caption = paste("Source: LCPS | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  
  theme(legend.position = c(0.5, 0.96),
        legend.background = element_rect(fill="#DAE3F3",size=0.8,linetype="solid"), #,colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=20, face="bold"))+
  
  theme(  plot.background = element_rect(fill = "#DAE3F3"), #background color/size (border color and size)
          plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
          plot.subtitle = element_text(hjust = 0.5,size = 20,face = "italic"),
          panel.background = element_rect(fill = "#DAE3F3", colour = "#DAE3F3"),
          axis.text = element_text(size=14,color = "black",face = "bold"),
          axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
          axis.ticks = element_line(colour = "#DAE3F3", size = 1, linetype = "solid"),
          axis.ticks.length = unit(0.5, "cm"),
          axis.line = element_line(colour = "#DAE3F3"),
          panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))

#ggsave("data/16c_hosp_new.png",width=19, height = 9)




# IC patients cumulative
ic.cumulative <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/intake-cumulative",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)


ic.cumulative$date <-as.Date(ic.cumulative$date)
ic.cumulative.1 <- ic.cumulative[ic.cumulative$date < "2020-07-01"]
ic.cumulative.2 <- ic.cumulative[ic.cumulative$date > "2020-07-01" & ic.cumulative$date < "2021-07-02"]
ic.cumulative.3 <- ic.cumulative[ic.cumulative$date > "2021-07-01"]

first.wave.total.ic <- last(ic.cumulative.1$value)
second.wave.total.ic <- (last(ic.cumulative.2$value)-first.wave.total.ic)




hospital.cumulative <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/intake-cumulative/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

hospital.cumulative$date <-as.Date(hospital.cumulative$date)
hospital.cumulative.1 <- hospital.cumulative[hospital.cumulative$date < "2020-07-01"]
hospital.cumulative.2 <- hospital.cumulative[hospital.cumulative$date > "2020-07-01" & hospital.cumulative$date > "2020-07-01"]
hospital.cumulative.3 <- hospital.cumulative[hospital.cumulative$date > "2021-07-01"]

first.wave.total <- last(hospital.cumulative.1$value)
second.wave.total <- (last(hospital.cumulative.2$value)-first.wave.total)

wave.compare <- data.frame(type="hosp", first=first.wave.total, second=second.wave.total)
wave.compare = rbind(wave.compare, data.frame(type="icu", first=first.wave.total.ic, second=second.wave.total.ic))


wave.compare.gather <- gather(wave.compare, first, second, -type)

colnames(wave.compare.gather) = c("type", "wave", "value")


wave.percentage <- round(((second.wave.total.ic+second.wave.total)/(first.wave.total.ic+first.wave.total))*100, digits = 1)
wave.subtitle <- paste("Eerste golfpercentage:") #,wave.percentage, "%")

wave.percentage.ic <- round(((second.wave.total.ic)/(first.wave.total.ic))*100, digits = 1)
wave.perc.ic <- paste("IC           ",wave.percentage.ic, "%")

wave.percentage.hosp <- round(((second.wave.total)/(first.wave.total))*100, digits = 1)
wave.perc.hosp <- paste("Kliniek ",wave.percentage.hosp, "%")


#### wave percentage plot #####

ggplot(wave.compare.gather, aes(wave, value, fill= type))+
  geom_bar(stat = "identity", position = "dodge")+
  ylab("")+
  xlab("")+
  
  scale_x_discrete(labels= c("Eerste golf", "Tweede golf"))+

  scale_fill_manual  (values=c("#F4B183","#4472C4"), labels=c(wave.perc.hosp, wave.perc.ic))+
  scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  labs(title="Aantal nieuwe opnames eerste golf  v. tweede golf", 
       subtitle=wave.subtitle,
       caption = paste("*datum einde eerste golf: 1 juli     |  Bron: NICE | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  
  theme(legend.position = "top",
        legend.direction = "vertical",
        legend.background = element_rect(fill="#DAE3F3",size=0.8,linetype="solid"), #,colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=20, face="bold"))+
  
  theme(  plot.background = element_rect(fill = "#DAE3F3"), #background color/size (border color and size)
          plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
          plot.subtitle = element_text(hjust = 0.5,size = 25,face = "bold"),
          panel.background = element_rect(fill = "#DAE3F3", colour = "#DAE3F3"),
          axis.text = element_text(size=14,color = "black",face = "bold"),
          axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
          axis.ticks = element_line(colour = "#DAE3F3", size = 1, linetype = "solid"),
          axis.ticks.length = unit(0.5, "cm"),
          axis.line = element_line(colour = "#DAE3F3"),
          panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))

ggsave("data/plots/16z_wave_ic-hosp.png",width=16, height = 9)





LCPS_datafeed_predict <- LCPS_datafeed
LCPS_datafeed_predict <- LCPS_datafeed_predict[order(LCPS_datafeed_predict$Datum ),]   #order by date

#### Calculate the 7 day MA per gemeente per day ####
 
LCPS_datafeed_predict <- LCPS_datafeed_predict %>% 
  mutate(MA_IC = rollapply(IC_Nieuwe_Opnames_COVID_Nederland, 7, mean, fill = NA, align = "right"))
LCPS_datafeed_predict <- LCPS_datafeed_predict %>% 
  mutate(MA_clin = rollapply(Kliniek_Nieuwe_Opnames_COVID_Nederland, 7, mean, fill = NA, align = "right"))


LCPS_datafeed_predict$MA_clin_lead_eight  <- lead(LCPS_datafeed_predict$MA_clin,7)
LCPS_datafeed_predict$percentage <- LCPS_datafeed_predict$MA_clin_lead_eight/LCPS_datafeed_predict$MA_clin
LCPS_datafeed_predict$peak <- LCPS_datafeed_predict$MA_clin/266.5714

LCPS_datafeed_predict$MA_IC_lead_eight  <- lead(LCPS_datafeed_predict$MA_IC,7)
LCPS_datafeed_predict$percentage.IC <- LCPS_datafeed_predict$MA_IC_lead_eight/LCPS_datafeed_predict$MA_IC
LCPS_datafeed_predict$peak.IC <- LCPS_datafeed_predict$MA_IC/53.571143


LCPS_datafeed_7days_ago  <- last(LCPS_datafeed_predict, 8)
LCPS_datafeed_7days_ago <- first(LCPS_datafeed_7days_ago,1)

IC.last.week <- LCPS_datafeed_7days_ago$MA_IC
clin.last.week <- LCPS_datafeed_7days_ago$MA_clin


LCPS_datafeed_predict$MAIC_rel_7da <- LCPS_datafeed_predict$MA_IC/IC.last.week
LCPS_datafeed_predict$MAClin_rel_7da <- LCPS_datafeed_predict$MA_clin/clin.last.week

### number for the tweet  ###
IC.perc.now <- round(last(LCPS_datafeed_predict$MAIC_rel_7da),2)*100
clin.perc.now <- round(last(LCPS_datafeed_predict$MAClin_rel_7da),2)*100

IC.perc.now <- as.integer(IC.perc.now)
clin.perc.now <- as.integer(clin.perc.now)


LCPS_datafeed_predict$MA_clin_lead  <- lead(LCPS_datafeed_predict$MA_clin,3)
LCPS_datafeed_predict$MA_IC_lead  <- lead(LCPS_datafeed_predict$MA_IC,3)


hosp_new_hosp.2 <- paste0("Aantal nieuwe opnames kliniek:  ",hosp.new.b1)



stap.een.df=data.frame(date=as.Date(c("2021-04-28")),event=c("Stap 1")) # - einde avondklok & terras open"))
stap.twee.old.df=data.frame(date=as.Date(c("2021-05-12")),event=c("Stap 2"))
stap.twee.df=data.frame(date=as.Date(c("2021-05-19")),event=c("Stap 2"))
stap.drie.df=data.frame(date=as.Date(c("2021-06-05")),event=c("Stap 3 - einde lockdown"))
stap.vier.df=data.frame(date=as.Date(c("2021-06-26")),event=c("Stap 4 & 5"))
# stap.vijf.df=data.frame(date=as.Date(c("2021-07-21")),event=c("Stap 5 - max 8 mensen thuis"))
stap.acht.df=data.frame(date=as.Date(c("2021-08-30")),event=c("Stap 6a - geen 1,5m in het onderwijs "))
stap.zes.df=data.frame(date=as.Date(c("2021-09-25")),event=c("Stap 6b - geen 1,5m meer "))
stap.zeven.df=data.frame(date=as.Date(c("2021-11-01")),event=c("Stap 6c - terug naar normaal "))

halve.maat.df=data.frame(date=as.Date(c("2021-11-02")),event=c("Halve maatregelen "))
stevige.klap.df=data.frame(date=as.Date(c("2021-11-12")),event=c("'stevige klap'"))
wertk.het.df=data.frame(date=as.Date(c("2021-11-28")),event=c("Avondclockdown"))





tomorrow <- Sys.Date()+1


LCPS_datafeed_predict2 <- LCPS_datafeed_predict[LCPS_datafeed_predict$Datum <= "2021-06-18",]



ggplot(LCPS_datafeed_predict)+

  
  geom_rect( aes(xmin = as.Date(today)-0.5,
                 xmax = as.Date(today)+0.5,
                 ymin = 0,
                 ymax = Inf,
  ), fill = "gray", alpha = 0.025)+
  
   geom_col(position = "dodge",  aes(x=Datum, y=Kliniek_Nieuwe_Opnames_COVID_Nederland), fill ="#c47945")+    
  
  
#  geom_vline(data=stap.een.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 0.5, color = "black")+
#  geom_text(data=stap.een.df  , mapping=aes(x=date, y=400, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
#  geom_vline(data=stap.twee.old.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 0.5, color = "black", alpha = 0.3)+
 # geom_text(data=stap.twee.old.df  , mapping=aes(x=date, y=400, label=event), size=5, angle=-90, vjust=-0.4, hjust=0, color= "black", alpha = 0.3)+
  
 #  geom_vline(data=stap.twee.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
 # geom_text(data=stap.twee.df  , mapping=aes(x=date, y=400, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
 # geom_vline(data=stap.drie.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
 # geom_text(data=stap.drie.df  , mapping=aes(x=date, y=495, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
 # geom_vline(data=stap.vier.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
 # geom_text(data=stap.vier.df  , mapping=aes(x=date, y=495, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  #geom_vline(data=stap.zes.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
 # geom_text(data=stap.zes.df  , mapping=aes(x=date, y=495, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  #geom_vline(data=stap.zeven.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "gray")+
  #geom_text(data=stap.zeven.df  , mapping=aes(x=date, y=295, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "gray")+
  
 # geom_vline(data=stap.acht.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
#  geom_text(data=stap.acht.df  , mapping=aes(x=date, y=495, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  
  
 # geom_vline(data=halve.maat.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
 # geom_text(data=halve.maat.df  , mapping=aes(x=date, y=495, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
 # geom_vline(data=stevige.klap.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
 # geom_text(data=stevige.klap.df  , mapping=aes(x=date, y=495, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
#  geom_vline(data=wertk.het.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
 # geom_text(data=wertk.het.df  , mapping=aes(x=date, y=495, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  

  
   geom_line(data = LCPS_datafeed_predict, aes(x=Datum, y=MA_clin_lead), size =3, color = "#DAE3F3")+
   geom_line(data = LCPS_datafeed_predict, aes(x=Datum, y=MA_clin_lead), size =2)+

    scale_x_date(date_breaks = "1 month", 
               date_labels= format("%b"),
               name="",
#               limits = as.Date(c("2020-10-18", NA)))+
               limits = as.Date(c("2020-10-17", "2022-02-20")))+
  
#  scale_y_continuous(limits = c(0, NA), labels = label_comma(big.mark = ".", decimal.mark = ","), breaks = c(0,12,40,80,100,200,300,400))+
scale_y_continuous(limits = c(0, 500), labels = label_comma(big.mark = ".", decimal.mark = ","), breaks = c(0,50,100,200,300,400,500))+
   
    geom_hline(yintercept=100, size = 1)+
    geom_hline(yintercept=40, size = 1)+

  annotate("text", x = as.Date("2022-02-18"), y = 105, label = "100 per dag", size=4,color = "black",face = "bold", hjust ="right")+
  annotate("text", x = as.Date("2022-02-18"), y = 45, label = "40 per dag", size=4,color = "black",face = "bold", hjust ="right")+
  
  annotate("text", x = as.Date("2022-02-18"), y = 175,  label = "Ernstig", size=10,color = "black",face = "bold", hjust ="right")+
  annotate("text", x = as.Date("2022-02-18"), y = 150, label = "(Landelijke maatregelen)", size=4,color = "black",face = "bold", hjust ="right")+
  
  annotate("text", x = as.Date("2022-02-18"), y = 80,  label = "Zorgelijk", size=10,color = "black",face = "bold", hjust ="right")+
  annotate("text", x = as.Date("2022-02-18"), y = 58, label = "(CoronaToegangsBewijs nodig)", size=4,color = "black",face = "bold", hjust ="right")+
  
  annotate("text", x = as.Date("2022-02-18"), y = 29,  label = "Waakzaam", size=10,color = "black",face = "bold", hjust ="right")+
  annotate("text", x = as.Date("2022-02-18"), y = 10, label = "(Geen CTB meer!)", size=4,color = "black",face = "bold", hjust ="right")+
  
  
  coord_cartesian(expand = FALSE)+
  
  ylab("")+
  labs(title=hosp_new_hosp.2, 
     # subtitle="Om een stap terug te doen, moet het aantal\n onder de signaalwaarde zitten voor twee weken",
       caption = paste("Bron: LCPS | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  
   theme(  plot.background = element_rect(fill = "#DAE3F3"), #background color/size (border color and size)
          plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
          plot.subtitle = element_text(hjust = 0.5,size = 15,face = "italic"),
          panel.background = element_rect(fill = "#DAE3F3", colour = "#DAE3F3"),
          axis.text = element_text(size=14,color = "black",face = "bold"),
          axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
          axis.ticks = element_line(colour = "#DAE3F3", size = 1, linetype = "solid"),
          axis.ticks.length = unit(0.5, "cm"),
          axis.line = element_line(colour = "#DAE3F3"),
          panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))+

ggsave("data/plots/16x_hosp_pred.png",width=16, height = 9)




#hosp_IC <- paste0("IC:            ",hosp.IC.b2, "   (", hosp.IC.c2 , ")")
hosp_new_IC.2 <- paste0("Aantal nieuwe opnames IC:  ",hosp.new.b2)




ggplot(LCPS_datafeed_predict)+
  
  
  geom_rect( aes(xmin = as.Date(today)-0.5,
                 xmax = as.Date(today)+0.5,
                 ymin = 0,
                 ymax = Inf,
  ), fill = "gray", alpha = 0.025)+
  
  geom_col(position = "dodge", aes(x=Datum, y=IC_Nieuwe_Opnames_COVID_Nederland), fill = "#4472C4")+
  


 # geom_vline(data=stap.een.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 0.5, color = "black")+
#  geom_text(data=stap.een.df  , mapping=aes(x=date, y=70, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
 
#  geom_vline(data=stap.twee.old.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 0.5, color = "black", alpha = 0.3)+
#  geom_text(data=stap.twee.old.df  , mapping=aes(x=date, y=70, label=event), size=5, angle=-90, vjust=-0.4, hjust=0, color= "black", alpha = 0.3)+
  
#  geom_vline(data=stap.twee.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
#  geom_text(data=stap.twee.df  , mapping=aes(x=date, y=79, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
#  geom_vline(data=stap.drie.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
#  geom_text(data=stap.drie.df  , mapping=aes(x=date, y=79, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
#  geom_vline(data=stap.vier.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
#  geom_text(data=stap.vier.df  , mapping=aes(x=date, y=79, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
#  geom_vline(data=stap.zes.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
#  geom_text(data=stap.zes.df  , mapping=aes(x=date, y=79, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  # geom_vline(data=stap.zeven.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "gray")+
  # geom_text(data=stap.zeven.df  , mapping=aes(x=date, y=45, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "gray")+
  
#  geom_vline(data=stap.acht.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
#  geom_text(data=stap.acht.df  , mapping=aes(x=date, y=79, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  
#  geom_vline(data=halve.maat.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
#  geom_text(data=halve.maat.df  , mapping=aes(x=date, y=79, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
#  geom_vline(data=stevige.klap.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
#  geom_text(data=stevige.klap.df  , mapping=aes(x=date, y=79, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
 # geom_vline(data=wertk.het.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
#  geom_text(data=wertk.het.df  , mapping=aes(x=date, y=79, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  
  
  
  
  geom_line(data = LCPS_datafeed_predict, aes(x=Datum, y=MA_IC_lead), size =3, color = "#DAE3F3")+
  geom_line(data = LCPS_datafeed_predict, aes(x=Datum, y=MA_IC_lead), size =2)+
  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%b"),
#               limits = as.Date(c("2020-10-18", NA)))+
               limits = as.Date(c("2020-10-17", "2022-02-20")))+
#  scale_y_continuous(limits = c(0, 80), breaks = c(0,3,10,20,30,50,70))+
   scale_y_continuous(limits = c(0, 80), breaks = c(0,10,20,30,40,50,60,70))+
  
  
    geom_hline(yintercept=25,  size = 1)+
    geom_hline(yintercept=10,  size = 1)+

  
  annotate("text", x = as.Date("2022-02-18"), y = 26, label = "25 per dag", size=4,color = "black",face = "bold", hjust ="right")+
  annotate("text", x = as.Date("2022-02-18"), y = 11, label = "10 per dag", size=4,color = "black",face = "bold", hjust ="right")+
 

  annotate("text", x = as.Date("2022-02-18"), y = 35,  label = "Ernstig", size=10,color = "black",face = "bold", hjust ="right")+
  annotate("text", x = as.Date("2022-02-18"), y = 32, label = "(Landelijke maatregelen)", size=4,color = "black",face = "bold", hjust ="right")+
  
  annotate("text", x = as.Date("2022-02-18"), y = 17.5,  label = "Zorgelijk", size=10,color = "black",face = "bold", hjust ="right")+
  annotate("text", x = as.Date("2022-02-18"), y = 14.5, label = "(CoronaToegangsBewijs nodig)", size=4,color = "black",face = "bold", hjust ="right")+
  
  annotate("text", x = as.Date("2022-02-18"), y = 5,  label = "Waakzaam", size=10,color = "black",face = "bold", hjust ="right")+
  annotate("text", x = as.Date("2022-02-18"), y = 3, label = "(Geen CTB meer!)", size=4,color = "black",face = "bold", hjust ="right")+
  
  
  coord_cartesian(expand = FALSE)+
  
  xlab("")+
  ylab("")+
  
  labs(title=hosp_new_IC.2, 
     # subtitle="Om een stap terug te doen, moet het aantal\n onder de signaalwaarde zitten voor twee weken",
       caption = paste("Bron: LCPS | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  
  theme(  plot.background = element_rect(fill = "#DAE3F3"), #background color/size (border color and size)
          plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
          plot.subtitle = element_text(hjust = 0.5,size = 15,face = "italic"),
          panel.background = element_rect(fill = "#DAE3F3", colour = "#DAE3F3"),
          axis.text = element_text(size=14,color = "black",face = "bold"),
          axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
          axis.ticks = element_line(colour = "#DAE3F3", size = 1, linetype = "solid"),
          axis.ticks.length = unit(0.5, "cm"),
          axis.line = element_line(colour = "#DAE3F3"),
          panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))+
  
  
ggsave("data/plots/16x_IC_pred.png",width=16, height = 9)




#### tweet.LCPS.tweet prep ####

hosp.new.b   <- as.integer(lcps.sh.3$IC_opname [2]+lcps.sh.3$clinic_opname[2])
hosp.total.b   <- as.integer(lcps.sh.1$IC_covid_nl [2]+lcps.sh.1$clinic_nl[2])
hosp.total.b1   <- as.integer(lcps.sh.1$clinic_nl[2])  #today
hosp.IC.b2   <- as.integer(lcps.sh.1$IC_covid_nl[2])  #today


deE <- intToUtf8(0x00EB)
deP <- intToUtf8(0x0025)

start.covid.nl = as.Date(c("2020-02-27"))
days.covid.in.nl = as.numeric(Sys.Date() - start.covid.nl+1)

if (hosp.total.b < hosp.total.a) {
  hosp.total.dot <- intToUtf8(0x1F7E2)     ### groen
} else if (hosp.total.b > hosp.total.a) {
  hosp.total.dot <- intToUtf8(0x1F534)     ### rood
} else
  hosp.total.dot <- intToUtf8(0x1F7E1)     ### geel

if (hosp.total.b1 < hosp.total.a1) {
  clinic.total.dot <- intToUtf8(0x1F7E2)     ### groen
} else if (hosp.total.b1 > hosp.total.a1) {
  clinic.total.dot <- intToUtf8(0x1F534)     ### rood
} else
  clinic.total.dot <- intToUtf8(0x1F7E1)     ### geel

if (hosp.IC.b2 < hosp.IC.a2) {
  ic.total.dot <- intToUtf8(0x1F7E2)     ### groen
} else if (hosp.IC.b2 > hosp.IC.a2) {
  ic.total.dot <- intToUtf8(0x1F534)     ### rood
} else
  ic.total.dot <- intToUtf8(0x1F7E1)     ### geel


if (hosp.new.b < hosp.new.a) {
  hosp.new.dot <- intToUtf8(0x1F7E2)     ### groen
} else if (hosp.new.b > hosp.new.a) {
  hosp.new.dot <- intToUtf8(0x1F534)     ### rood
} else
  hosp.new.dot <- intToUtf8(0x1F7E1)     ### geel


hosp.new.b1 <-as.integer(hosp.new.b1)
hosp.new.b2 <-as.integer(hosp.new.b2)

hosp.cl.new.dot <- intToUtf8(0x1F7E1)     ### geel
if (hosp.new.b1 > 79) {
  hosp.cl.new.dot <- intToUtf8(0x1F534)     ### rood
}else if (hosp.new.b1 < 13) {
  hosp.cl.new.dot <- intToUtf8(0x1F7E2)     ### groen
} 


hosp.ic.new.dot <- intToUtf8(0x1F7E1)     ### geel
if (hosp.new.b2 > 19) {
  hosp.ic.new.dot <- intToUtf8(0x1F534)     ### rood
} else if (hosp.new.b2 < 4) {
  hosp.ic.new.dot <- intToUtf8(0x1F7E2)     ### groen
} 





#### tweet.LCPS.tweet ####



hosp.total.b  <- format( hosp.total.b,  big.mark="." ,decimal.mark=",")
hosp.total.b1 <- format( hosp.total.b1, big.mark="." ,decimal.mark=",")
#hosp.total.b2 <- format( hosp.total.b2, big.mark="." ,decimal.mark=",")
hosp.new.b    <- format( hosp.new.b,    big.mark="." ,decimal.mark=",")

flag.D <- intToUtf8(0x1F1E9)
flag.E <- intToUtf8(0x1F1EA)

heart.emoji <- intToUtf8(0x2764)
deP <- intToUtf8(0x0025)

clin.tehoog <- round(hosp.new.b1 / 12 , 0)
ic.tehoog   <- round(hosp.new.b2 / 3  , 0)

hosp.total.new.today <- hosp.new.b1+ hosp.new.b2

#### Risico nieuw
# <4 - waakzaam = 70 per week = >10 per dag
# 4-16 - zorgelijk = 70-280 p/w = 10-40 p/d
# 16-27 - ernstig = 280-472,5 p/w = 40-67,5 p/d
# > 27 - zeer ernstig = 472,5 p/w - > 67 p/d

hosp.new.b1
hosp.new.b2



#### inschaling routekaart ####

kliniek.lvl <- last(LCPS_datafeed_predict$MA_clin_lead,4)
kliniek.lvl <- first(kliniek.lvl, 1)
ic.lvl <- last(LCPS_datafeed_predict$MA_IC_lead,4)
ic.lvl <- first(ic.lvl, 1)




hosp.new.dot <- intToUtf8(0x1F7E1)     ### geel
if (kliniek.lvl > 100 || ic.lvl > 25) {
  hosp.new.dot <- intToUtf8(0x1F534)     ### rood
} else if (kliniek.lvl < 40 & ic.lvl < 10) {
  hosp.new.dot <- intToUtf8(0x1F7E2)     ### groen
} 


hosp.new.lvl <- "Zorgelijk"
if (kliniek.lvl > 100 || ic.lvl > 25) {
  hosp.new.lvl <- "Ernstig"     
} else if (kliniek.lvl < 40 & ic.lvl < 10) {
  hosp.new.lvl <-  "Waakzaam"   
} 




tweet.LCPS.EN.tweet <- "Dag %s, de %s editie

Pati%snten nu in het ziekenhuis:
(het verschil met gisteren)

%s%s (%s)

Kliniek:  %s (%s)
IC:       %s (%s)

%s%s%s


Nieuwe opnames: 
Kliniek: %s 
IC:        %s

Huidig risiconiveau:
%s%s%s

#COVID19" 


tweet.LCPS.EN.tweet <- sprintf(tweet.LCPS.EN.tweet,
                            days.covid.in.nl, editionname,
                            deE,
                            hosp.total.dot,   hosp.total.b,     hosp.total.c,
                           
                            hosp.total.b1,  hosp.total.c1,
                            hosp.IC.b2,  hosp.IC.c2,
                            flag.D,flag.E,inDE,

                            
                            hosp.new.b1,   
                            hosp.new.b2,
                            
                            hosp.new.dot, hosp.new.lvl , hosp.new.dot
)
                            

Encoding(tweet.LCPS.EN.tweet) <- "UTF-8"

 post_tweet(tweet.LCPS.EN.tweet,  media = c("data/plots/16a_IC_hosp.png",
                                           "data/plots/16b_IC_only.png",
                                           "data/plots/16x_hosp_pred.png",
                                           "data/plots/16x_IC_pred.png"
                                            ))




source("C:\\Rdir\\Rscripts\\01_lcps_data-NICE_graph.R")



#### tweet.NICE.NEW.tweet ####

tweet.NICE.NEW.tweet <- "Opnames per week - NICE"

tweet.NICE.NEW.tweet <- sprintf(tweet.NICE.NEW.tweet)
Encoding(tweet.NICE.NEW.tweet) <- "UTF-8"

post_tweet(tweet.NICE.NEW.tweet,  media = c("data/plots/77_NICE_age_hosp_per_week.png",
                                            "data/plots/77_NICE_age_hosp_per_week_rel.png",
                                            "data/plots/77_NICE_age_IC_per_week.png",
                                            "data/plots/77_NICE_age_IC_per_week_rel.png" 
), in_reply_to_status_id = get_reply_id())




#### tweet.hospital.effect.tweet ####

tweet.hospital.effect.tweet <- "Opnames"

tweet.hospital.effect.tweet <- sprintf(tweet.hospital.effect.tweet)
Encoding(tweet.hospital.effect.tweet) <- "UTF-8"

 post_tweet(tweet.hospital.effect.tweet,  media = c("data/plots/70_vaccinated_compare_age_clinic_abs.png",
                                                    "data/plots/16x_omt_check_nice_peak.png",
                                                  # "data/plots/70_vaccinated_compare_age_clinic.png",
                                                   "data/plots/71_vaccinated_compare_age_ICU_abs.png",
                                                  # "data/plots/71_vaccinated_compare_age_IC.png"
                                                  "data/plots/16x_omt_check_week_on_week.png" 
 ), in_reply_to_status_id = get_reply_id())

 
 
 
 #### tweet.afschaling.tweet ####
 
 tweet.afschaling.tweet <- "Afschaling zorg.
 Minimum eerste golf: 345 bedden non-covid"
 
 tweet.afschaling.tweet <- sprintf(tweet.afschaling.tweet)
 Encoding(tweet.afschaling.tweet) <- "UTF-8"
 
 post_tweet(tweet.afschaling.tweet,  media = c("data/plots/16b_IC_only_reverse.png"
                                               ), in_reply_to_status_id = get_reply_id())
 
 
 
 
 
 

 
 




                    