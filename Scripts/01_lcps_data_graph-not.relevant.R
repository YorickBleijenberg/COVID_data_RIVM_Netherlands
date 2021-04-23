
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

#import from LCPS website

#LCPS_datafeed<-read.csv("https://lcps.nu/wp-content/uploads/covid-19.csv",sep=",")  

LCPS_datafeed<-read.csv("C:\\Rdir\\data\\plots\\covid-19.csv",sep=",")  

LCPS_datafeed$Datum <- as.Date(LCPS_datafeed$Datum ,format="%d-%m-%Y")
#LCPS_datafeed$week<-strftime(LCPS_datafeed$Datum,format = "%V")


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
colnames(lcps_working_1) <- c("date", "IC_covid_nl", "IC_neg" ,"clinic_nl","IC_opname", "clinic_opname")

##### prepare for combination #####

today<-strftime(Sys.Date(),format = "%Y-%m-%d")
lcps_working_1 <- lcps_working_1[lcps_working_1$date>"2020-10-17"&lcps_working_1$date<=today,]
lcps_working_old <- lcps_working_old[lcps_working_old$date>"2020-02-26"&lcps_working_old$date<="2020-10-17",]



##### data for IC graph  ####


lcps_working_1a  <- lcps_working_1[,c("date","IC_covid_nl","IC_neg")]
lcps_working_2a  <- lcps_working_old[,c("date","IC_covid_nl","IC_neg")]

lcps.both.df <- rbind(lcps_working_2a, lcps_working_1a)

colnames(lcps.both.df) <- c("date", "B_IC_covid_nl", "A_IC_neg")

lcps_working_2_long<- gather(lcps.both.df, "B_IC_covid_nl","A_IC_neg", -date)
colnames(lcps_working_2_long) <- c("date", "type", "number")



####  IC plot ####

lcps.both.df <-lcps.both.df[order(lcps.both.df$date),]
lcps.sh.2 <- tail(lcps.both.df,n=2)

hosp.total.x   <- as.integer(lcps.sh.2$B_IC_covid_nl [1]+lcps.sh.2$A_IC_neg[1])  #yesterday
hosp.total.y   <- as.integer(lcps.sh.2$B_IC_covid_nl [2]+lcps.sh.2$A_IC_neg[2])  #today
hosp.total.z   <- hosp.total.y - hosp.total.x
hosp.total.y <- format(hosp.total.y, big.mark="." ,decimal.mark=",")
hosp_title.a <- paste0("Aantal patienten op de IC: ", hosp.total.y, "  (", hosp.total.z , ")")

hosp.total.x1   <- as.integer(lcps.sh.2$A_IC_neg[1])  #yesterday
hosp.total.y1   <- as.integer(lcps.sh.2$A_IC_neg[2])  #today
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
                      breaks=c(250,500,800,1150,1250,1350,1450,1550),
                      sec.axis = dup_axis()
                      )+

  
  scale_fill_manual  (values=c("#C5E0B4","#4472C4"), labels=c(hosp_clin.a, hosp_IC.a))+
  scale_color_manual(values=c("#767171","#3B3838"), labels=c(hosp_clin.a, hosp_IC.a))+
    
    labs(#title=hosp_title.a, # subtitle=" ",
         caption = paste("Source: LCPS & NICE | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  
  theme(legend.position = "none")+
  
  theme(  plot.background = element_rect(fill = "#DAE3F3"), #background color/size (border color and size)
          plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
          panel.background = element_rect(fill = "#DAE3F3", colour = "#DAE3F3"),
          axis.text = element_text(size=14,color = "black",face = "bold"),
          axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
          axis.ticks = element_line(colour = "#DAE3F3", size = 1, linetype = "solid"),
          axis.ticks.length = unit(0.5, "cm"),
          axis.line = element_line(colour = "#DAE3F3"),
          panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))

ggsave("data/plots/16b_IC_only.png",width=16, height = 9)





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
hosp.total.c   <- hosp.total.b - hosp.total.a
hosp.total.b <- format(hosp.total.b, big.mark="." ,decimal.mark=",")
hosp_title <- paste0("Aantal patienten met COVID-19 nu in het ziekenhuis: ", hosp.total.b, "  (", hosp.total.c , ")")

hosp.total.a1   <- as.integer(lcps.sh.1$clinic_nl[1])  #yesterday
hosp.total.b1   <- as.integer(lcps.sh.1$clinic_nl[2])  #today
hosp.total.c1   <- hosp.total.b1 - hosp.total.a1
hosp.total.b1  <- format( hosp.total.b1, big.mark="." ,decimal.mark=",")
hosp_clin <- paste0("Kliniek: ",hosp.total.b1, "  (", hosp.total.c1 , ")")

hosp.IC.a2   <- as.integer(lcps.sh.1$IC_covid_nl[1])  #yesterday
hosp.IC.b2   <- as.integer(lcps.sh.1$IC_covid_nl[2])  #today
hosp.IC.c2   <- hosp.IC.b2 - hosp.IC.a2
hosp.IC.b2  <- format( hosp.IC.b2, big.mark="." ,decimal.mark=",")
hosp_IC <- paste0("IC:            ",hosp.IC.b2, "   (", hosp.IC.c2 , ")")



ggplot(data = lcps_working_tot_c_1_long, mapping = aes(x = date, y = number, color = type, fill = type))+
  geom_bar(stat='identity')+
  
  scale_x_date(name="")+
  ylab("")+
  scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","))+
  scale_fill_manual (values=c("#F4B183","#4472C4"), labels=c(hosp_clin, hosp_IC))+
  scale_color_manual(values=c("#843C0C","#3B3838"), labels=c(hosp_clin, hosp_IC))+
  
  labs(#title = hosp_title, # subtitle=" ",
       caption = paste("Source: LCPS | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  
  theme(legend.position = c(0.5, 0.9),
        legend.background = element_rect(fill="#DAE3F3",size=0.8,linetype="solid"), #,colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=20, face="bold"))+
  
  theme(legend.position = "none")+
  
  theme(  plot.background = element_rect(fill = "#DAE3F3"), #background color/size (border color and size)
          plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
          panel.background = element_rect(fill = "#DAE3F3", colour = "#DAE3F3"),
          axis.text = element_text(size=14,color = "black",face = "bold"),
          axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
          axis.ticks = element_line(colour = "#DAE3F3", size = 1, linetype = "solid"),
          axis.ticks.length = unit(0.5, "cm"),
          axis.line = element_line(colour = "#DAE3F3"),
        panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))

ggsave("data/plots/16a_IC_hosp.png",width=16, height = 9)





#### data for NEW hosp graph  ####

lcps_working_1a  <- lcps_working_1[,c("date","IC_opname","clinic_opname")]
lcps_working_2a  <- lcps_working_old[,c("date","IC_opname","clinic_opname")]

lcps.both.df <- rbind(lcps_working_2a, lcps_working_1a)

#lcps.both.df <- gather(lcps.both.df, "IC_opname","clinic_opname", -date)
#colnames(lcps.both.df) <- c("date", "type", "number")


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


##### geom_stream  ######


ggplot(data = lcps_working_2_long, mapping = aes(x = date, y = number, fill = type)) + 
  geom_stream(bw=0.08)+
  
  scale_x_date(name="")+

  scale_y_continuous(name=" ", breaks=c(-800,-400,0,400,800),
                     labels=c("800", "400", "0", "400","800"))+
  
#   scale_fill_manual(values=c("#4472C4","#C5E0B4"), name=" test ",labels=c("IC - covid", "IC - non-covid"))+
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

 #  ggsave("data/17_IC_only-2.png",width=16, height = 9)




# IC patients cumulative
ic.cumulative <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/intake-cumulative",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)


ic.cumulative$date <-as.Date(ic.cumulative$date)
ic.cumulative.1 <- ic.cumulative[ic.cumulative$date < "2020-07-01"]
ic.cumulative.2 <- ic.cumulative[ic.cumulative$date > "2020-07-01"]

first.wave.total.ic <- last(ic.cumulative.1$value)
second.wave.total.ic <- (last(ic.cumulative.2$value)-first.wave.total.ic)




hospital.cumulative <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/intake-cumulative/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

hospital.cumulative$date <-as.Date(hospital.cumulative$date)
hospital.cumulative.1 <- hospital.cumulative[hospital.cumulative$date < "2020-07-01"]
hospital.cumulative.2 <- hospital.cumulative[hospital.cumulative$date > "2020-07-01"]

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
  mutate(MA_IC = rollapply(IC_Nieuwe_Opnames_COVID, 7, mean, fill = NA, align = "right"))
LCPS_datafeed_predict <- LCPS_datafeed_predict %>% 
  mutate(MA_clin = rollapply(Kliniek_Nieuwe_Opnames_COVID, 7, mean, fill = NA, align = "right"))

LCPS_datafeed_predict$MA_clin_lead  <- lead(LCPS_datafeed_predict$MA_clin,3)
LCPS_datafeed_predict$MA_IC_lead  <- lead(LCPS_datafeed_predict$MA_IC,3)



hosp_new_hosp.2 <- paste0("Aantal nieuwe opnames kliniek:  ",hosp.new.b1)



stap.een.df=data.frame(date=as.Date(c("2021-04-28")),event=c("Stap 1 - einde avondklok & terras open"))
stap.twee.df=data.frame(date=as.Date(c("2021-05-11")),event=c("Stap 2 - weer naar buiten!"))
stap.drie.df=data.frame(date=as.Date(c("2021-05-26")),event=c("Stap 3 - weer uit eten & naar de bios!"))
stap.vier.df=data.frame(date=as.Date(c("2021-06-16")),event=c("Stap 4 - max 6 mensen thuis & evenementen"))
stap.vijf.df=data.frame(date=as.Date(c("2021-07-07")),event=c("Stap 5 - max 8 mensen thuis"))
stap.zes.df=data.frame(date=as.Date(c("2021-08-15")),event=c("Stap 6 - terug naar normaal"))


tomorrow <- Sys.Date()+1


ggplot(LCPS_datafeed_predict)+

   geom_col(position = "dodge",  aes(x=Datum, y=Kliniek_Nieuwe_Opnames_COVID ), fill ="#4472C4")+       #"#F4B183")+  
  
  
  annotate("rect", xmin = as.Date("2020-10-18"), xmax =as.Date("2021-05-20"), ymin =80, ymax = Inf, color = "black",fill = "red", alpha = 0.4)+
  annotate("rect", xmin = as.Date("2020-10-18"), xmax =as.Date("2021-05-20"), ymin =40, ymax = 80, color = "black",fill = "orange", alpha = 0.5)+
  annotate("rect", xmin = as.Date("2020-10-18"), xmax =as.Date("2021-05-20"), ymin =12, ymax = 40, color = "black",fill = "yellow", alpha = 0.4)+
  annotate("rect", xmin = as.Date("2020-10-18"), xmax =as.Date("2021-05-20"), ymin =0, ymax = 12, color = "black",fill = "green", alpha = 0.3)+ 
  
  
  
  geom_vline(data=stap.een.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
  geom_text(data=stap.een.df  , mapping=aes(x=date, y=400, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  geom_vline(data=stap.twee.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
  geom_text(data=stap.twee.df  , mapping=aes(x=date, y=400, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  
  
   geom_line(aes(x=Datum, y=MA_clin_lead), size =3, color = "#DAE3F3")+
   geom_line(aes(x=Datum, y=MA_clin_lead), size =2)+

    scale_x_date(date_breaks = "1 month", 
               date_labels= format("%d %b"),
               name="",
               limits = as.Date(c("2020-10-18", NA)))+
  
  scale_y_continuous(limits = c(0, NA), labels = label_comma(big.mark = ".", decimal.mark = ","), breaks = c(0,12,40,80,100,200,300,400))+
  
    geom_hline(yintercept=80, size = 1)+
    geom_hline(yintercept=40, size = 1)+
    geom_hline(yintercept=12, size = 1)+
  
  #annotate("text", x = as.Date("2020-10-18"), y = 86, label = "80 per dag", size=4,color = "black",face = "bold", hjust ="left")+
  #annotate("text", x = as.Date("2020-10-18"), y = 46, label = "40 per dag", size=4,color = "black",face = "bold", hjust ="left")+
  #annotate("text", x = as.Date("2020-10-18"), y = 18, label = "12 per dag", size=4,color = "black",face = "bold", hjust ="left")+
  
  coord_cartesian(expand = FALSE)+
  
  ylab("")+
  labs(#title=hosp_new_hosp.2, 
       #subtitle="Om een stap terug te doen, moet het aantal\n onder de signaalwaarde zitten voor twee weken",
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
  
#  geom_text( aes( x=as.Date("2020-12-18"), y=390, label="besmet rond kerst"),
#             color="black", 
#             size=7 , angle=0, fontface="bold")+
  
#  annotate("curve", x = as.Date("2020-12-25"), xend =as.Date("2021-01-04"), 
#           y = 390, yend = 310, curvature = -0.2,
#           colour = "black", size=2, alpha=0.7, arrow =arrow(type = "open",length = unit(2,"mm")))
  
  
ggsave("data/plots/16x_hosp_pred.png",width=16, height = 9)

#hosp_IC <- paste0("IC:            ",hosp.IC.b2, "   (", hosp.IC.c2 , ")")
hosp_new_IC.2 <- paste0("Aantal nieuwe opnames IC:  ",hosp.new.b2)






ggplot(LCPS_datafeed_predict)+
  
  
  geom_col(position = "dodge", aes(x=Datum, y=IC_Nieuwe_Opnames_COVID ), fill = "#4472C4")+
  
  
  annotate("rect", xmin = as.Date("2020-10-18"), xmax =as.Date("2021-05-20"), ymin =20, ymax = Inf, color = "black",fill = "red", alpha = 0.4)+
  annotate("rect", xmin = as.Date("2020-10-18"), xmax =as.Date("2021-05-20"), ymin =10, ymax = 20, color = "black",fill = "orange", alpha = 0.5)+
  annotate("rect", xmin = as.Date("2020-10-18"), xmax =as.Date("2021-05-20"), ymin =3, ymax = 10, color = "black",fill = "yellow", alpha = 0.4)+
  annotate("rect", xmin = as.Date("2020-10-18"), xmax =as.Date("2021-05-20"), ymin =0, ymax = 3, color = "black",fill = "green", alpha = 0.3)+ 

  
  geom_vline(data=stap.een.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
  geom_text(data=stap.een.df  , mapping=aes(x=date, y=70, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  geom_vline(data=stap.twee.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
  geom_text(data=stap.twee.df  , mapping=aes(x=date, y=70, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  
  
  geom_line(aes(x=Datum, y=MA_IC_lead), size =3, color = "#DAE3F3")+
  geom_line(aes(x=Datum, y=MA_IC_lead), size =2)+
  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%d %b"),
               limits = as.Date(c("2020-10-18", NA)))+
  
  scale_y_continuous(limits = c(0, 80), breaks = c(0,3,10,20,30,50,70))+
  
  geom_hline(yintercept=20,  size = 1)+
  geom_hline(yintercept=10,  size = 1)+
  geom_hline(yintercept=3,   size = 1)+
  
  #annotate("text", x = as.Date("2020-10-15"), y = 21, label = "20 per dag", size=4,color = "black",face = "bold", hjust ="left")+
  #annotate("text", x = as.Date("2020-10-15"), y = 11, label = "10 per dag", size=4,color = "black",face = "bold", hjust ="left")+
  #annotate("text", x = as.Date("2020-10-15"), y = 4,  label = "3 per dag", size=4,color = "black",face = "bold", hjust ="left")+

  coord_cartesian(expand = FALSE)+
  
  xlab("")+
  ylab("")+
  
  labs(#title=hosp_new_IC.2, 
  #    subtitle="Om een stap terug te doen, moet het aantal\n onder de signaalwaarde zitten voor twee weken",
       caption = paste("Bron: LCPS | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  
  
  #annotate("text", x = as.Date("2020-10-11"), y = 15, label = "Ernstig", size=8,color = "black",face = "bold", hjust ="left")+
  #annotate("text", x = as.Date("2020-10-11"), y = 7, label = "Zorgelijk", size=8,color = "black",face = "bold", hjust ="left")+
  #annotate("text", x = as.Date("2020-10-11"), y = 1.5,  label = "Waakzaam", size=7,color = "black",face = "bold", hjust ="left")+
  
  
  #annotate("text", x = as.Date("2020-10-10"), y = 11.25, label = "10 per dag", size=4,color = "black",face = "bold", hjust ="left")+
  #annotate("text", x = as.Date("2020-10-10"), y = 4.25, label = "3 per dag", size=4,color = "black",face = "bold", hjust ="left")+
  
  
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










#lcps.both.df.verhoud <-lcps.both.df

#lcps.both.df.verhoud$verhoud <- lcps.both.df.verhoud$clinic_nl/lcps.both.df.verhoud$IC_covid_nl

#lcps.both.df.verhoud  <-  lcps.both.df.verhoud %>% arrange(date)

#lcps.both.df.verhoud$verhoud_MA  <- rollmeanr(lcps.both.df.verhoud$verhoud, 7, fill = 0)



#ggplot(lcps.both.df.verhoud)+
 #   geom_point(aes(x=date, y=verhoud), size =3, color = "#000000", alpha=0.5)+
#  geom_line(aes(x=date, y=verhoud_MA), size =3, color = "#000000")+
               
#  scale_x_date(date_breaks = "1 month", 
#               date_labels= format("%d %b"),
#               limits = as.Date(c("2020-03-10", NA)))+
#  scale_y_continuous(limits = c(0, 7))+
#  labs(title="Bezetting: verhouding kliniek/IC")#  , breaks = c(0,3,10,20,40,60))+

#ggsave("data/IC_verhouding.png",width=16, height = 9)



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
if (hosp.new.b1 > 40) {
  hosp.cl.new.dot <- intToUtf8(0x1F534)     ### rood
}else if (hosp.new.b1 < 12) {
  hosp.cl.new.dot <- intToUtf8(0x1F7E2)     ### groen
} 


hosp.ic.new.dot <- intToUtf8(0x1F7E1)     ### geel
if (hosp.new.b2 > 10) {
  hosp.ic.new.dot <- intToUtf8(0x1F534)     ### rood
} else if (hosp.new.b2 < 3) {
  hosp.ic.new.dot <- intToUtf8(0x1F7E2)     ### groen
} 



#### tweet.LCPS.tweet ####



#Als ik de regering was, zou ik het ook niet hebben over de doden.
#want: "elke dode is er één teveel".  
#en hier zie je eigenlijk gewoon de kosten van falend beleid in mensenlevens.


#### de-kosten-van-falend-corona-beleid-zijn-mensenlevens.
#### we-zijn-er-nog-lang-niet

hosp.total.b  <- format( hosp.total.b,  big.mark="." ,decimal.mark=",")
hosp.total.b1 <- format( hosp.total.b1, big.mark="." ,decimal.mark=",")
#hosp.total.b2 <- format( hosp.total.b2, big.mark="." ,decimal.mark=",")
hosp.new.b    <- format( hosp.new.b,    big.mark="." ,decimal.mark=",")

flag.D <- intToUtf8(0x1F1E9)
flag.E <- intToUtf8(0x1F1EA)

heart.emoji <- intToUtf8(0x2764)


clin.tehoog <- round(hosp.new.b1 / 12 , 0)
ic.tehoog   <- round(hosp.new.b2 / 3  , 0)


tweet.LCPS.EN.tweet <- "Day %s, The %s edition

Patients currently in the hospital:
(difference with yesterday)

%s%s (%s)

%sClinic:  %s (%s)
%sICU:       %s (%s)

New: 
%sClinic:  %s (%sx #TooHigh)
%sICU:     %s (%sx #TooHigh)


#COVID19" # https://www.youtube.com/watch?v=KpYhePFx1qo"


tweet.LCPS.EN.tweet <- sprintf(tweet.LCPS.EN.tweet,
                            days.covid.in.nl, editionname,
                            
                            hosp.total.dot,   hosp.total.b,     hosp.total.c,
                           
                            clinic.total.dot, hosp.total.b1,  hosp.total.c1,
                            ic.total.dot,     hosp.IC.b2,  hosp.IC.c2,
                          
                            hosp.cl.new.dot, hosp.new.b1,    clin.tehoog, # hosp.new.c1,
                            hosp.ic.new.dot, hosp.new.b2,     ic.tehoog    # hosp.new.c2,
#                            flag.D, flag.E,
#                            number.in.DE
                            )
Encoding(tweet.LCPS.EN.tweet) <- "UTF-8"

post_tweet(tweet.LCPS.EN.tweet,  media = c("data/plots/16a_IC_hosp.png",
                                           "data/plots/16b_IC_only.png",
                                           "data/plots/16x_hosp_pred.png",
                                           "data/plots/16x_IC_pred.png"
                                           ))




tweet.LCPS.tweet <- "Dag %s, de %s editie

Pati%snten nu in het ziekenhuis:
(het verschil met gisteren)

%s (%s)

Kliniek:  %s (%s)
IC:          %s (%s)

Nieuwe opnames:
Kliniek:    %s (%sx #TeHoog)
IC:        %s (%sx #TeHoog)

#COVID19 #TeHoog "#https://www.youtube.com/watch?v=KpYhePFx1qo"


tweet.LCPS.tweet <- sprintf(tweet.LCPS.tweet,
                            days.covid.in.nl, editienaam,
                            deE, 
                            
                            hosp.total.b,     hosp.total.c,
                            hosp.total.b1,    hosp.total.c1,
                            hosp.IC.b2,       hosp.IC.c2,
                            
                            hosp.new.b1,   clin.tehoog,
                            hosp.new.b2,   ic.tehoog
)
Encoding(tweet.LCPS.tweet) <- "UTF-8"

lcps_file <- paste0("data/00_", today , "_lcsp.txt")
write.table(tweet.LCPS.tweet, file = lcps_file, sep = "\t",row.names = FALSE) #, col.names = NA)














                    