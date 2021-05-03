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
 stap.twee.df=data.frame(date=as.Date(c("2021-05-18")),event=c("Stap 2 - weer naar buiten!"))
 stap.drie.df=data.frame(date=as.Date(c("2021-06-02")),event=c("Stap 3 - weer uit eten & naar de bios!"))
 stap.vier.df=data.frame(date=as.Date(c("2021-06-23")),event=c("Stap 4 - max 6 mensen thuis & evenementen"))
 stap.vijf.df=data.frame(date=as.Date(c("2021-07-14")),event=c("Stap 5 - max 8 mensen thuis"))
 stap.zes.df=data.frame(date=as.Date(c("2021-08-18")),event=c("Stap 6 - terug naar normaal"))
 
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


##  Merged_data_short$cases <- format(Merged_data_short$cases, big.mark="." ,decimal.mark=",")


 #df.predict.lead.kerst$MACases_lead  <- lead(df.predict.lead.kerst$MACases,3)
 #df.predict.lead.kerst$MACases_2_lead  <- lead(df.predict.lead.kerst$MACases,3)


###   k%u0336 e%u0336  r%u0336s%u0336t%u0336
#e.strike <- intToUtf8(0x0336)
#strike.tekst <- "k%u0336e%u0336r%u0336s%u0336t%u0336"
#Encoding(e.strike) <- "UTF-8"

#title.kerst <- paste("De 'kunnen we Independence Day vieren?' grafiek")

title.kerst <- paste("De 'halen we de stappen?' grafiek")


#### prediction plot ####
  
ggplot(Merged_data_short)+
    geom_bar(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"))+     #, color = "#96afde"
    scale_fill_manual(values=c("#96afde"))+
 
  # geom_point(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"))+
  

  ### stoplicht
 annotate("rect", xmin = as.Date("2020-12-01"), xmax =as.Date("2021-08-30"), ymin =6250, ymax = Inf, color = "black",fill = "red", alpha = 0.6)+
 annotate("rect", xmin = as.Date("2020-12-01"), xmax =as.Date("2021-08-30"), ymin =2500, ymax = 6250, color = "black",fill = "orange", alpha = 0.5)+
 annotate("rect", xmin = as.Date("2020-12-01"), xmax =as.Date("2021-08-30"), ymin =875, ymax = 2500, color = "black",fill = "yellow", alpha = 0.4)+
 annotate("rect", xmin = as.Date("2020-12-01"), xmax =as.Date("2021-08-30"), ymin =0, ymax = 875, color = "black",fill = "green", alpha = 0.3)+ 
  
  ### ECDC kleur
  annotate("rect", xmin = as.Date("2020-12-15"), xmax =as.Date("2021-08-30"), ymin =0, ymax = 315, color = "black",fill = "green", alpha = 0.3)+ 
   
  ### routekaart kleuren ###
 # annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2020-12-31"), ymin =6250, ymax = Inf, color = "black",fill = "#68032F", alpha = 1)+
 # annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2020-12-31"), ymin =3750, ymax = 6250, color = "black",fill = "#BC2166", alpha = 1)+
 # annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2020-12-31"), ymin =1250, ymax = 3750, color = "black",fill = "#DB5C94", alpha = 1)+
 # annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2020-12-31"), ymin =0, ymax = 1250, color = "black",fill = "#F291BC", alpha = 1)+ 
  
 # geom_point(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"))+
   
  annotate("text", x = as.Date("2020-12-02"), y = 7500, label = "Zeer Ernstig", size=10,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-12-02"), y = 5000, label = "Ernstig", size=10,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-12-02"), y = 2000, label = "Zorgelijk", size=10,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-12-02"), y = 550,  label = "Waakzaam", size=10,color = "black",face = "bold", hjust ="left")+
  
  annotate("text", x = as.Date("2020-12-02"), y = 6360, label = "250/100K/week - 6250", size=2,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-12-02"), y = 2600, label = "100/100K/week - 2500", size=2,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-12-02"), y = 1000, label = "35/100K/week - 875", size=2,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2021-10-26"), y = 450, label = "25/100K/ 2 weken - 315", size=2,color = "black",face = "bold", hjust ="left")+
  
  
   ## ECDC
  annotate("text", x = as.Date("2020-09-18"), y = 600,  label = "ECDC niveau 'groen'", size=4,color = "black",face = "bold", hjust ="left")+
  annotate("curve", x = as.Date("2020-10-20"), xend =as.Date("2020-10-25"), 
           y = 600, yend = 375, curvature = -0.2,
           colour = "black", size=2, alpha=0.8, arrow =arrow(type = "closed",length = unit(2,"mm")))+
  
   ## prediction lines
####  geom_line(data=df.daling.4, aes(x=Datum, y=r7), size = 1.25, color = "black")+
 #  geom_line(data=df.daling.4, aes(x=Datum, y=r8))+
 #  geom_line(data=df.daling.4, aes(x=Datum, y=r9))+
 ## geom_line(data=df.daling.4, aes(x=Datum, y=r10), size = 1.25, color = "darkgray")+
 ## geom_line(data=df.daling.4, aes(x=Datum, y=r12), size = 1.25, color = "darkgray")+
###geom_line(data=df.daling.4, aes(x=Datum, y=r14), size = 1.25, color = "lightgray", linetype = "dashed" )+
  # geom_line(data=df.daling.4, aes(x=Datum, y=r16))+
####  geom_line(data=df.daling.4, aes(x=Datum, y=r18))+     #waakzaam met kerst
 # geom_line(data=df.daling.4, aes(x=Datum, y=r15))+
  # geom_line(data=df.daling.4, aes(x=Datum, y=r105))+    # ECDC groen met kerst
#####  geom_line(data=df.daling.4, aes(x=Datum, y=r21), size = 1.25, color = "black")+
########geom_line(data=df.daling.4, aes(x=Datum, y=r28), size = 1.25, color = "gray", linetype = "dashed" )+

 #######geom_line(data=df.daling.4, aes(x=Datum, y=r_12_4), size = 1.25, color = "black", linetype = "dashed" )+
 #######geom_line(data=df.daling.4, aes(x=Datum, y=r_26_3), size = 1.25, color = "black", linetype = "dashed" )+  
 

 
  geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#F5F5F5",lwd = 4)+
  geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#44546a",lwd = 3)+
  
 # geom_vline(xintercept = as.Date("2020-12-25"), linetype = "dashed", size = 1.5, color = "darkgreen")+
 
 
 # geom_vline(data=sint.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "black")+
 # geom_text(data=sint.df  , mapping=aes(x=date, y=10000, label=event), size=5, angle=-90, vjust=0.4, hjust=0)+
  
#######   PersCo  ####
  
#   geom_vline(data=sint.persco.df,  mapping=aes(xintercept=date), linetype = "dotted", size = 1, color = "black")+
 #  geom_text(data=sint.persco.df  , mapping=aes(x=date, y=7500, label=event), size=4, angle=-90, vjust = -0.5, hjust=0, color= "black")+
  
 # geom_vline(data=kerst.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "darkgreen")+
 # geom_text(data=kerst.df  , mapping=aes(x=date, y=6500, label=event), size=8, angle=90, vjust=-0.4, hjust=0)+
  
 # geom_vline(data=sint.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "black")+
 # geom_text(data=sint.df  , mapping=aes(x=date, y=10000, label=event), size=4, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
 # geom_vline(data=sint.persco.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "black")+
 # geom_text(data=sint.persco.df  , mapping=aes(x=date, y=16000, label=event), size=+6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  #geom_vline(data=kerst.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "darkgreen")+
 # geom_text(data=kerst.df  , mapping=aes(x=date, y=12500, label=event), size=8, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
 
   #geom_text(data=thanks.df, mapping=aes(x=as.Date("2020-11-26"), y=3850, label=thanks), size=8, angle=90, vjust=-0.4, hjust=0)+
  
  
  geom_line(data=df.predict.lead.kerst, aes(x=fixedDate, y=MACases), size = 1, color = "darkred")+
  geom_point(data=df.predict.lead.kerst, aes(x=fixedDate, y=MACases), size = 2, color = "black")+
  geom_point(data=df.predict.lead.kerst, aes(x=fixedDate, y=MACases), size = 1.5, color = "darkred")+
 # geom_line(data=df.predict.lead.kerst, aes(x=Date, y=MACases_2), size = 0.6, color = "black")+ 
  geom_point(data=df.predict.lead.kerst, aes(x=Date, y=MACases_2), size = 1.5, color = "#F5F5F5", alpha  =0.75)+ 
  geom_point(data=df.predict.lead.kerst, aes(x=Date, y=MACases_2), size = 1, color = "#44546a", alpha  =0.75)+ 
  
  #scale 11.500
#####  annotate("text", x = as.Date("2021-04-06"), y = 5000,  label = " R = 0.8", size=5,angle=-70, color = "black",face = "bold", hjust ="left")+
##### # annotate("text", x = as.Date("2021-04-19"), y = 5000,  label = " R = 0.9", size=5,angle=-45, color = "black",face = "bold", hjust ="left")+
  ##annotate("text", x = as.Date("2020-12-30"), y = 8000,  label = "halvering elke 4 weken", size=5,angle=-60, color = "black",face = "bold", hjust ="left")+
  #annotate("text", x = as.Date("2020-11-06"), y = 6000,  label = "halvering elke 7 dagen", size=5,angle=-80, color = "black",face = "bold", hjust ="left")+
  #annotate("text", x = as.Date("2020-11-16"), y = 5000,  label = "Waakzaam tijdens kerst", size=5,angle=-55, color = "black",face = "bold", hjust ="left")+
  
  
 ######  annotate("text", x = as.Date("2020-11-11"), y = 8500,  label = "Doel kabinet:\nhalvering elke 4 weken", size=5,angle=-45, color = "black",face = "bold", hjust ="left")+
 ## annotate("text", x = as.Date("2020-11-05"), y = 7000,  label = "halvering elke 7 dagen", size=5,angle=-80, color = "black",face = "bold", hjust ="left")+
 ## annotate("text", x = as.Date("2020-11-17"), y = 5000,  label = "Waakzaam tijdens kerst", size=5,angle=-45, color = "black",face = "bold", hjust ="left")+
  
 # geom_vline(data=carnaval.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "black")+
 # geom_text(data=carnaval.df  , mapping=aes(x=date, y=12000, label=event), size=8, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  #geom_vline(data=verkiezingen.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "black")+
  #geom_text(data=verkiezingen.df  , mapping=aes(x=date, y=12000, label=event), size=8, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  
 # geom_vline(data=pasen.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "black")+
 # geom_text(data=pasen.df  , mapping=aes(x=date, y=12000, label=event), size=8, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  geom_vline(data=stap.een.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 0.5, color = "black", alpha = 0.7)+
  geom_text(data=stap.een.df  , mapping=aes(x=date, y=12000, label=event), size=7, angle=-90, vjust=-0.4, hjust=0, color= "black", alpha = 0.7)+
  
  
  geom_vline(data=stap.twee.old.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 0.5, color = "black", alpha = 0.2)+
  geom_text(data=stap.twee.old.df  , mapping=aes(x=date, y=12000, label=event), size=5, angle=-90, vjust=-0.4, hjust=0, color= "black", alpha = 0.2)+

  geom_vline(data=stap.twee.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
  geom_text(data=stap.twee.df  , mapping=aes(x=date, y=12000, label=event), size=7, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  geom_vline(data=stap.drie.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
  geom_text(data=stap.drie.df  , mapping=aes(x=date, y=12000, label=event), size=7, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  geom_vline(data=stap.vier.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
  geom_text(data=stap.vier.df  , mapping=aes(x=date, y=12000, label=event), size=7, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  geom_vline(data=stap.vijf.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
  geom_text(data=stap.vijf.df  , mapping=aes(x=date, y=12000, label=event), size=7, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  geom_vline(data=stap.zes.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
  geom_text(data=stap.zes.df  , mapping=aes(x=date, y=12000, label=event), size=7, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  
  
  
  
 # geom_vline(data=independence.day.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "black")+
 # geom_text(data=independence.day.df  , mapping=aes(x=date, y=12000, label=event), size=8, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  

  
  # annotate("text", x = as.Date("2020-11-16"), y = 5500,  label = paste( doublingdayZ.1.text, "elke",doublingdayZ.1.int, "dagen"), size=5,angle=-40, color = "black",face = "bold", hjust ="left")+
  # annotate("text", x = as.Date("2020-11-16"), y = 4000,  label = paste( doublingdayZ.text, "elke",doublingdayZ.int, "dagen"), size=5,angle=-50, color = "black",face = "bold", hjust ="left")+
  
  theme_classic()+
    xlab("")+
    ylab("")+
   
  #scale_y_continuous()+ #trans='log2')+
    #lims(x= c(NA, NA), y = c(16, NA))+
    #ylim(16, NA)+
 
  scale_y_continuous(limits = c(0, 13500),breaks = c(2500, 5000, 7500,10000,12500,15000), labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  coord_cartesian(expand = FALSE)+
  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%b"),
               limits = as.Date(c("2020-12-01", "2021-08-30")))+
  
  labs(title = title.kerst,
         subtitle = "met 7-daags voortschrijdend gemiddelde",
         caption = paste("Bron: RIVM | Plot: @YorickB  | ",Sys.Date()))+
  theme(
      plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
      panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
      legend.position = "none",   # no legend
      plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
      plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
      axis.text = element_text(size=14,color = "black",face = "bold"),
      axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
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
  
  ggsave("data/plots/60_trendlines_cases.png",width=16, height = 9)

  
  
  
  

#### prediction plot ####

ggplot(Merged_data_short)+
  
  
  ### routekaart kleuren ###
   annotate("rect", xmin = as.Date("2020-12-01"), xmax =as.Date("2021-08-30"), ymin =6250, ymax = Inf, color = "black",fill = "#68032F", alpha = 1)+
   annotate("rect", xmin = as.Date("2020-12-01"), xmax =as.Date("2021-08-30"), ymin =2500, ymax = 6250, color = "black",fill = "#BC2166", alpha = 1)+
   annotate("rect", xmin = as.Date("2020-12-01"), xmax =as.Date("2021-08-30"), ymin =875, ymax = 2500, color = "black",fill = "#DB5C94", alpha = 1)+
  annotate("rect", xmin = as.Date("2020-12-01"), xmax =as.Date("2021-08-30"), ymin =0, ymax = 875, color = "black",fill = "#F291BC", alpha = 1)+ 
  
  # geom_point(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"))+
  
  annotate("text", x = as.Date("2020-12-01"), y = 7500, label = "Zeer Ernstig", size=10,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-12-01"), y = 5000, label = "Ernstig", size=10,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-12-01"), y = 2000, label = "Zorgelijk", size=10,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-12-01"), y = 550,  label = "Waakzaam", size=10,color = "black",face = "bold", hjust ="left")+
  
  annotate("text", x = as.Date("2020-12-02"), y = 6360, label = "250/100K/week - 6250", size=2,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-12-02"), y = 2600, label = "100/100K/week - 2500", size=2,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-12-02"), y = 1000, label = "35/100K/week - 875", size=2,color = "black",face = "bold", hjust ="left")+
 # annotate("text", x = as.Date("2020-10-26"), y = 390, label = "25/100K/ 2 weken", size=2,color = "black",face = "bold", hjust ="left")+
  
  
  geom_bar(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"), fill = "gray", alpha = 0.35)+     #, color = "#96afde"
 # scale_fill_manual(values=c("gray"), alpha = 0.3)+
  

  ###  7day MA  
  geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#F5F5F5",lwd = 3)+
  geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#44546a",lwd = 2)+
  
  # geom_vline(xintercept = as.Date("2020-12-25"), linetype = "dashed", size = 1.5, color = "darkgreen")+
  
#  geom_vline(data=sint.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "black")+
#  geom_text(data=sint.df  , mapping=aes(x=date, y=10000, label=event), size=4, angle=-90, vjust=-0.4, hjust=0, color= "#F5F5F5")+
  
### PersCo ######
  
#   geom_vline(data=sint.persco.df,  mapping=aes(xintercept=date), linetype = "dotted", size = 1, color = "#F5F5F5")+
 #  geom_text(data=sint.persco.df  , mapping=aes(x=date, y=7500, label=event), size=4, angle=-90, vjust=-0.4, hjust=0, color= "#F5F5F5")+
  
  ##geom_vline(data=kerst.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "darkgreen")+
 ## geom_text(data=kerst.df  , mapping=aes(x=date, y=10000, label=event), size=8, angle=-90, vjust=-0.4, hjust=0, color= "#F5F5F5")+
  
#  geom_vline(data=verkiezingen.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "white")+
#  geom_text(data=verkiezingen.df  , mapping=aes(x=date, y=12000, label=event), size=8, angle=-90, vjust=-0.4, hjust=0, color= "white")+
  
#    geom_vline(data=carnaval.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "white")+
#    geom_text(data=carnaval.df  , mapping=aes(x=date, y=12000, label=event), size=8, angle=-90, vjust=-0.4, hjust=0, color= "white")+
    
 #   geom_vline(data=pasen.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "white")+
 #   geom_text(data=pasen.df  , mapping=aes(x=date, y=12000, label=event), size=8, angle=-90, vjust=-0.4, hjust=0, color= "white")+
    
    geom_vline(data=stap.een.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 0.5, color = "white", alpha = 0.7)+
    geom_text(data=stap.een.df  , mapping=aes(x=date, y=12000, label=event), size=7, angle=-90, vjust=-0.4, hjust=0, color= "white", alpha = 0.7)+
    
  geom_vline(data=stap.twee.old.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 0.5, color = "white", alpha = 0.2)+
  geom_text(data=stap.twee.old.df  , mapping=aes(x=date, y=12000, label=event), size=5, angle=-90, vjust=-0.4, hjust=0, color= "white", alpha = 0.2)+
  
  geom_vline(data=stap.twee.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "white")+
  geom_text(data=stap.twee.df  , mapping=aes(x=date, y=12000, label=event), size=7, angle=-90, vjust=-0.4, hjust=0, color= "white")+
  
    geom_vline(data=stap.drie.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "white")+
  geom_text(data=stap.drie.df  , mapping=aes(x=date, y=12000, label=event), size=7, angle=-90, vjust=-0.4, hjust=0, color= "white")+
  
  geom_vline(data=stap.vier.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "white")+
  geom_text(data=stap.vier.df  , mapping=aes(x=date, y=12000, label=event), size=7, angle=-90, vjust=-0.4, hjust=0, color= "white")+
  
  geom_vline(data=stap.vijf.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "white")+
  geom_text(data=stap.vijf.df  , mapping=aes(x=date, y=12000, label=event), size=7, angle=-90, vjust=-0.4, hjust=0, color= "white")+
  
  geom_vline(data=stap.zes.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "white")+
  geom_text(data=stap.zes.df  , mapping=aes(x=date, y=12000, label=event), size=7, angle=-90, vjust=-0.4, hjust=0, color= "white")+
  
  
  
  
  theme_classic()+
  xlab("")+
  ylab("")+

  scale_y_continuous(limits = c(0, 13500),breaks = c(2500, 5000, 7500,10000, 12500), labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  coord_cartesian(expand = FALSE)+
  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%b"),
               limits = as.Date(c("2020-12-01", "2021-08-30")))+
  
  labs(title = "Routekaartkleuren",
       subtitle = "met 7-daags voortschrijdend gemiddelde",
       caption = paste("Bron: RIVM | Plot: @YorickB  | ",Sys.Date()))+
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    legend.position = "none",   # no legend
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    #scale_x_date(),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    # axis.text.x=element_blank(),
    axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_line(colour= "lightgray", linetype = "dashed")
  )+ 
  
  #  panel.grid.major.y = element_line(colour= "lightgray"))  #, linetype = "dashed"))
  ggsave("data/plots/60_routekaart.png",width=16, height = 9)
  











