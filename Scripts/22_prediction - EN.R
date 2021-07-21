library(tidyverse)

###  ECDC
###  150  - 75    - 1872
###  50   - 25    -  624
###  25   - 12.5  -  312

###  NL
###  250      -  6240
###  150      -  3744
###  100      -  2496
###   50      -  1248
###   35      -   874

###   Merged_data_2$ma_c_lead  <- lead(Merged_data_2$MACases,3)


Merged_data_7MA <- Merged_data_2


#Gewenste dagen subsetten
Merged_data_short <- Merged_data_7MA[Merged_data_7MA$dateInTable>"2020-07-01"&Merged_data_7MA$dateInTable<=Sys.Date(),]
Merged_data_short$observation <- 1:nrow(Merged_data_short)

Merged_data_short$fixedDate <- as.Date(Merged_data_short$dateInTable,format="%Y-%m-%d")







#df.predict.lead.2  <-  last(Merged_data_short)

df.predict.lead  <-  tail(Merged_data_short,4)
df.predict.lead  <-  df.predict.lead[1,]
df.predict.lead  <- df.predict.lead[ -c(1:4,6:8,11:17)]

today <- as.Date(df.predict.lead$fixedDate)
pred.MAcases  <- df.predict.lead$ma_c_lead
pred.gfc      <- as.integer(tail(Merged_data_short$gf_c, 1))

today  <-   Sys.Date() # as.Date("2021-01-18")  
kersmis.Dag <- as.Date("2020-12-25")
sint.Dag <- as.Date("2020-12-05")
carnaval.Dag <- as.Date("2021-02-14")

verkiezings.Dag <- as.Date("2021-03-17")
bevrijdings.Dag <- as.Date("2021-05-05")

independence.day <- as.Date("2021-07-04")

days.to.sint <- as.vector(difftime(sint.Dag, today, units='days'))
days.to.kerst <- as.vector(difftime(kersmis.Dag, today, units='days'))

days.to.elections <- as.vector(difftime(verkiezings.Dag, today, units='days'))
days.to.freedom <- as.vector(difftime(bevrijdings.Dag, today, units='days'))
days.to.independence <- as.vector(difftime(independence.day, today, units='days'))




e <- Working_Set$MACases[3]
ea <- Working_Set$MACases[2]
f <- Working_Set$MACases[1]

gf.c.d <- (df.predict.lead$gf_c[1]/7)


doublingdayZ     <- (log(2)/(log(e/f)/7))         
doublingdayZ.2   <-  exp(log(2)/doublingdayZ)*100

doublingdayZ.1     <- (log(2)/(log(e/ea)))             
doublingdayZ.3   <-  exp(log(2)/doublingdayZ.1)*100


df.predict.lead.kerst <- df.predict.lead
df.predict.lead.kerst$NumDays <- c(1)
df.predict.lead.kerst$MACases_2 <- c(ea)
df.predict.lead.kerst$MACases <- c(f)
df.predict.lead.kerst$Date <- c(today-7)   ####  10  ######


i=2 
while (i < 365) {
ma.cases.pred <- df.predict.lead.kerst$MACases[i-1]  /100*doublingdayZ.2
ma.cases.pred.2 <- df.predict.lead.kerst$MACases_2[i-1]/100*doublingdayZ.3
df.predict.lead.kerst <- add_row(df.predict.lead.kerst, NumDays = i, MACases = ma.cases.pred, MACases_2 = ma.cases.pred.2)
i <- i+1
}

df.predict.lead.kerst$MACases <- as.integer(df.predict.lead.kerst$MACases)
df.predict.lead.kerst$MACases_2 <- as.integer(df.predict.lead.kerst$MACases_2)
df.predict.lead.kerst$Date <- today+df.predict.lead.kerst$NumDays-5
df.predict.lead.kerst$fixedDate <- today+df.predict.lead.kerst$NumDays-11



### import daling and set emojis  #####

df.daling.4 <- read.csv("C:\\Rdir\\data-contstant\\daling-2.csv" ,sep=";")
colnames(df.daling.4) = c("Datum", "r7", "r8","r9", "r10", "r12", "r14", "r16", "r21", "r28","r18", "r15","r105","r_12_4", "r_26_3")
df.daling.4$Datum <- as.Date(df.daling.4$Datum)

emoji_kerst <- intToUtf8(0x1F384)
emoji_snowman <- intToUtf8(0x2603)
emoji_snow <- intToUtf8(0x2744)
emoji_santa <- intToUtf8(0x1F385)
emoji_clown <- intToUtf8(0x1F921)
emoji_rabbit <- intToUtf8(0x1F407)
emoji_ballotbox <- intToUtf8(0x1F5F3)


### press events                         
#dates_vline <- as.Date(c("2020-09-18", "2020-09-28", "2020-10-13", "2020-11-04"))
#dates_vline <- which((df4$Datum %in% dates_vline))

kerst <- paste("kerst  ",emoji_kerst,emoji_snowman,emoji_snow )
Encoding(kerst) <- "UTF-8"

#sinter <- paste("Sinterklaas",emoji_santa)
sint.persco<- paste("PersCo")

carnaval <- paste("Carnaval  ",emoji_clown )
Encoding(carnaval) <- "UTF-8"

pasen <- paste("pasen  ",emoji_rabbit )
Encoding(pasen) <- "UTF-8"


verkiezingen <- paste("verkiezingen  ",emoji_ballotbox)
Encoding(verkiezingen) <- "UTF-8"

 #sint.df =data.frame(date=as.Date(c("2020-12-05")),event=c(sinter))
 
sint.persco.df =data.frame(date=as.Date(c("2021-04-13")),event=c(sint.persco))
 
kerst.df=data.frame(date=as.Date(c("2020-12-25")),event=c(kerst))
 
 carnaval.df=data.frame(date=as.Date(c("2021-02-14", "2021-02-16")),event=c(" ", carnaval))
 
  verkiezingen.df=data.frame(date=as.Date(c("2021-03-17")),event=c(verkiezingen))
 
  pasen.df=data.frame(date=as.Date(c("2021-04-04")),event=c(pasen))
 
 Bevrijdingsdag.df=data.frame(date=as.Date(c("2021-05-05")),event=c("Bevrijdingsdag"))
 
 
 stap.een.df=data.frame(date=as.Date(c("2021-04-28")),event=c("Stap 1")) # - einde avondklok & terras open"))
 stap.twee.old.df=data.frame(date=as.Date(c("2021-05-11")),event=c("Stap 2 - weer naar buiten!"))
 stap.twee.df=data.frame(date=as.Date(c("2021-05-19")),event=c("Stap 2"))
 stap.drie.df=data.frame(date=as.Date(c("2021-06-05")),event=c("End of lockdown"))

 stap.vier.df=data.frame(date=as.Date(c("2021-06-26")),event=c("Almost all restrictions lifted"))
 stap.vier.old.df=data.frame(date=as.Date(c("2021-06-30")),event=c("Stap 4 & 5 "))
 
 stap.vijf.df=data.frame(date=as.Date(c("2021-07-21")),event=c("Stap 5 - max 8 mensen thuis"))

 
 stap.terug.df=data.frame(date=as.Date(c("2021-07-09")),event=c("No Night life"))
 
 stap.zes.old.df=data.frame(date=as.Date(c("2021-09-01")),event=c("Stap 6"))
 stap.zes.df=data.frame(date=as.Date(c("2021-08-14")),event=c("Stap 6 - terug naar normaal"))
 
 independence.day.df=data.frame(date=as.Date(c("2021-07-04")),event=c("Independence Day"))

 
 
 
  


doublingdayZ.int = as.integer(abs(doublingdayZ))
doublingdayZ.1.int = as.integer(abs(doublingdayZ.1))
if (doublingdayZ<0){
  doublingdayZ.text = "halvering"
} else{
  doublingdayZ.text = "verdubbeling"
}
if (doublingdayZ.1<0){
  doublingdayZ.1.text = "halvering"
} else{
  doublingdayZ.1.text = "verdubbeling"
}  



title.kerst <- paste("Summer turned out to be different in the Netherlands")



Merged_data_short$MACases14  <- rollmeanr(Merged_data_short$cases, 14, fill = 0)
Merged_data_short$MACases14_lead  <- lead(Merged_data_short$MACases14,6)




















#### prediction plot ####
  
ggplot(Merged_data_short)+
   
  
  # geom_point(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"))+
  

  ### stoplicht
 #annotate("rect", xmin = as.Date("2021-06-01"), xmax =as.Date("2021-07-08"), ymin =6250, ymax = Inf, color = "black",fill = "red", alpha = 0.6)+
 #annotate("rect", xmin = as.Date("2021-06-01"), xmax =as.Date("2021-07-06"), ymin =2500, ymax = 6250, color = "black",fill = "orange", alpha = 0.5)+
 annotate("rect", xmin = as.Date("2021-06-01"), xmax =as.Date("2021-09-15"), ymin =625, ymax = 2500, color = "black",fill = "yellow", alpha = 0.4)+
 #annotate("rect", xmin = as.Date("2021-06-01"), xmax =as.Date("2021-09-15"), ymin =0, ymax = 875, color = "black",fill = "green", alpha = 0.3)+ 
  
  
  
  ### ECDC kleur
  annotate("rect", xmin = as.Date("2021-04-01"), xmax =as.Date("2021-07-30"), ymin =6250, ymax = Inf, color = "black",fill = "red", alpha = 0.7)+ 
  annotate("rect", xmin = as.Date("2021-04-01"), xmax =as.Date("2021-07-30"), ymin =2500, ymax = 6250, color = "black",fill = "red", alpha = 0.3)+ 
  annotate("rect", xmin = as.Date("2021-04-01"), xmax =as.Date("2021-07-30"), ymin =625, ymax = 2500, color = "black",fill = "yellow", alpha = 0.4)+
    annotate("rect", xmin = as.Date("2021-04-01"), xmax =as.Date("2021-07-30"), ymin =0, ymax = 625, color = "black",fill = "green", alpha = 0.3)+ 

  geom_bar(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"))+     #, color = "#96afde"
  scale_fill_manual(values=c("black"))+
  
   
  annotate("text", x = as.Date("2021-07-29"), y = 7500, label = "Dark Red", size=10,color = "black",face = "bold", hjust ="right")+
  annotate("text", x = as.Date("2021-07-29"), y = 4375, label = "Red", size=10,color = "black",face = "bold", hjust ="right")+  #5000
  annotate("text", x = as.Date("2021-07-29"), y = 1500,  label = "Yellow", size=9,color = "black",face = "bold", hjust ="right")+
  annotate("text", x = as.Date("2021-07-29"), y = 300,  label = " green", size=9,color = "black",face = "bold", hjust ="right")+
  
  
  annotate("text", x = as.Date("2021-07-29"), y = 6350, label = "500/100K/14-days - 6250", size=3,color = "black",face = "bold", hjust ="right")+
  annotate("text", x = as.Date("2021-07-29"), y = 2600, label = "200/100K/14-days - 2500", size=3,color = "black",face = "bold", hjust ="right")+
  annotate("text", x = as.Date("2021-07-29"), y = 725, label = "50/100K/14-days - 625", size=3,color = "black",face = "bold", hjust ="right")+


#        geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#F5F5F5",lwd = 4)+   #7 day
#        geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#44546a",lwd = 3)+
  
  geom_line(mapping = aes(x=fixedDate, y=MACases14_lead), color = "#F5F5F5",lwd = 4)+   #14 day
  geom_line(mapping = aes(x=fixedDate, y=MACases14_lead), color = "#44546a",lwd = 3)+
  
  
  
  
#  geom_vline(data=stap.een.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 0.5, color = "black", alpha = 0.7)+
#  geom_text(data=stap.een.df  , mapping=aes(x=date, y=6950, label=event), size=7, angle=-90, vjust=-0.4, hjust=0, color= "black", alpha = 0.7)+
  
#  geom_vline(data=stap.twee.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
#  geom_text(data=stap.twee.df  , mapping=aes(x=date, y=6950, label=event), size=7, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
    

## geom_vline(data=stap.terug.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
##  geom_text(data=stap.terug.df  , mapping=aes(x=date, y=12000, label=event), size=7, angle=-90, vjust=-0.4, hjust=0, color= "#F5F5F5")+
 
   
##    geom_vline(data=stap.drie.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
##  geom_text(data=stap.drie.df  , mapping=aes(x=date, y=12000, label=event), size=7, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
##  geom_vline(data=stap.vier.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
##  geom_text(data=stap.vier.df  , mapping=aes(x=date, y=12000, label=event), size=7, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  
  
  theme_classic()+
    xlab("")+
    ylab("")+
   

 
  #scale_y_continuous(limits = c(0, 5000),breaks = c(500, 1000, 1500,2000,2500,3000), labels = label_comma(big.mark = ".", decimal.mark = ","))+
  scale_y_continuous(limits = c(0, 12500),breaks = c(2500, 5000, 7500, 10000, 12500, 15000,17500,20000), labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  
    
  coord_cartesian(expand = FALSE)+
  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%b"),
               limits = as.Date(c("2021-04-01", "2021-07-30")))+
  
  labs(title = title.kerst,
         subtitle = "With 14 day moving average",
         caption = paste("Bron: RIVM | Plot: @YorickB  | ",Sys.Date()))+
  theme(
      plot.background = element_rect(fill = "#F5F5F5"), 
      panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
      legend.position = "none",  
      plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
      plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
      axis.text = element_text(size=14,color = "black",face = "bold"),
      axis.text.y = element_text(face="bold", color="black", size=14), 
      axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
      axis.ticks.length = unit(0.5, "cm"),
      axis.line = element_line(colour = "#F5F5F5"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
  #    panel.grid.major.x = element_line(colour= "lightgray", linetype = "dashed")
   #   panel.grid.major.y = element_line(colour= "lightgray")  #, linetype = "dashed"))
      )+
  
 # geom_hline( aes(yintercept=2123))+
#  geom_vline( aes(xintercept=as.Date("2021-02-11")))+
  
#  geom_hline( aes(yintercept=3629))+
  
ggsave("data/plots/60_trendlines_cases_En_anno.png",width=16, height = 9)

  

  