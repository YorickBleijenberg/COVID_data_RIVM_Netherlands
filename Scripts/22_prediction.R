library(tidyverse)

###  ECDC
###  150  - 75    - 1872
###  50   - 25    -  624
###  25   - 12.5  -  312

###  NL
###  250      -  6240
###  150      -  3744
###   50      -  1248
###   35      -   874



Merged_data_7MA <- Merged_data_2


#Gewenste dagen subsetten
Merged_data_short <- Merged_data_7MA[Merged_data_7MA$dateInTable>"2020-07-01"&Merged_data_7MA$dateInTable<=Sys.Date(),]
Merged_data_short$observation <- 1:nrow(Merged_data_short) 


Merged_data_short$fixedDate <- as.Date(Merged_data_short$dateInTable,format="%Y-%m-%d")




#df.predict.2  <-  last(Merged_data_short)

df.predict  <-  tail(Merged_data_short,1)
df.predict  <- df.predict[ -c(1:4,6:8,10:14)]

today <- as.Date(df.predict$fixedDate)
pred.MAcases  <- df.predict$MACases
pred.gfc      <- as.integer(df.predict$gf_c)

today  <- Sys.Date()
kersmis.Dag <- as.Date("2020-12-25")
sint.Dag <- as.Date("2020-12-05")
carnaval.Dag <- as.Date("2021-02-14")

days.to.sint <- as.vector(difftime(sint.Dag, today, units='days'))
days.to.kerst <- as.vector(difftime(kersmis.Dag, today, units='days'))

e <- Working_Set$MACases[3]
ea <- Working_Set$MACases[2]
f <- Working_Set$MACases[1]

gf.c.d <- (df.predict$gf_c[1]/7)


doublingdayZ     <- (log(2)/(log(e/f)/7))         
doublingdayZ.2   <-  exp(log(2)/doublingdayZ)*100

doublingdayZ.1     <- (log(2)/(log(e/ea)))             
doublingdayZ.3   <-  exp(log(2)/doublingdayZ.1)*100


df.predict.kerst <- df.predict
df.predict.kerst$NumDays <- c(1)
df.predict.kerst$MACases_2 <- c(ea)
df.predict.kerst$MACases <- c(f)
df.predict.kerst$Date <- c(today-7)


i=2 
while (i < 365) {
ma.cases.pred <- df.predict.kerst$MACases[i-1]  /100*doublingdayZ.2
ma.cases.pred.2 <- df.predict.kerst$MACases_2[i-1]/100*doublingdayZ.3
df.predict.kerst <- add_row(df.predict.kerst, NumDays = i, MACases = ma.cases.pred, MACases_2 = ma.cases.pred.2)
i <- i+1
}

df.predict.kerst$MACases <- as.integer(df.predict.kerst$MACases)
df.predict.kerst$MACases_2 <- as.integer(df.predict.kerst$MACases_2)
df.predict.kerst$Date <- today+df.predict.kerst$NumDays-2
df.predict.kerst$fixedDate <- today+df.predict.kerst$NumDays-8



### import daling and set emojis  #####

df.daling.4 <- read.csv("C:\\Rdir\\data-contstant\\daling-2.csv" ,sep=";")
colnames(df.daling.4) = c("Datum", "r7", "r8","r9", "r10", "r12", "r14", "r16", "r21", "r28","r18", "r15","r105")
df.daling.4$Datum <- as.Date(df.daling.4$Datum)
emoji_kerst <- intToUtf8(0x1F384)
emoji_snowman <- intToUtf8(0x2603)
emoji_snow <- intToUtf8(0x2744)
emoji_santa <- intToUtf8(0x1F385)
emoji_clown <- intToUtf8(0x1F921)

### press events                         
#dates_vline <- as.Date(c("2020-09-18", "2020-09-28", "2020-10-13", "2020-11-04"))
#dates_vline <- which((df4$Datum %in% dates_vline))

kerst <- paste("kerst  ",emoji_kerst,emoji_snowman,emoji_snow )
Encoding(kerst) <- "UTF-8"

sinter <- paste("Sinterklaas",emoji_santa)
sint.persco<- paste("PersCo - Versoepeling / lockdown (?)")


carnaval <- paste("Carnaval  ",emoji_clown )
Encoding(kerst) <- "UTF-8"



 sint.df =data.frame(date=as.Date(c("2020-12-05")),event=c(sinter))
 sint.persco.df =data.frame(date=as.Date(c("2020-12-08")),event=c(sint.persco))
 kerst.df=data.frame(date=as.Date(c("2020-12-25")),event=c(kerst))
 
 carnaval.df=data.frame(date=as.Date(c("2021-02-14", "2021-02-16")),event=c(" ", carnaval))
 
 


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

#### prediction plot ####
  
ggplot(Merged_data_short)+
    geom_bar(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"))+     #, color = "#96afde"
    scale_fill_manual(values=c("#96afde"))+
 
  ### stoplicht
 annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2021-02-05"), ymin =6250, ymax = Inf, color = "black",fill = "red", alpha = 0.6)+
 annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2021-02-05"), ymin =3750, ymax = 6250, color = "black",fill = "orange", alpha = 0.5)+
 annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2021-02-05"), ymin =1250, ymax = 3750, color = "black",fill = "yellow", alpha = 0.4)+
 annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2021-02-05"), ymin =0, ymax = 1250, color = "black",fill = "green", alpha = 0.3)+ 
  
  ### ECDC kleur
  annotate("rect", xmin = as.Date("2020-08-01"), xmax =as.Date("2021-02-05"), ymin =0, ymax = 315, color = "black",fill = "green", alpha = 0.3)+ 
   
  ### routekaart kleuren ###
 # annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2020-12-31"), ymin =6250, ymax = Inf, color = "black",fill = "#68032F", alpha = 1)+
 # annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2020-12-31"), ymin =3750, ymax = 6250, color = "black",fill = "#BC2166", alpha = 1)+
 # annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2020-12-31"), ymin =1250, ymax = 3750, color = "black",fill = "#DB5C94", alpha = 1)+
 # annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2020-12-31"), ymin =0, ymax = 1250, color = "black",fill = "#F291BC", alpha = 1)+ 
  
 # geom_point(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"))+
   
  annotate("text", x = as.Date("2020-07-05"), y = 7500, label = "Zeer Ernstig", size=10,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-07-05"), y = 5000, label = "Ernstig", size=10,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-07-05"), y = 2500, label = "Zorgelijk", size=10,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-07-05"), y = 600,  label = "Waakzaam", size=10,color = "black",face = "bold", hjust ="left")+
  
  annotate("text", x = as.Date("2020-07-02"), y = 6360, label = "250/100K/week", size=2,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-07-02"), y = 3860, label = "150/100K/week", size=2,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-07-02"), y = 1320, label = "50/100K/week", size=2,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-10-26"), y = 390, label = "25/100K/ 2 weken", size=2,color = "black",face = "bold", hjust ="left")+
  
  
   ## ECDC
  annotate("text", x = as.Date("2020-09-27"), y = 600,  label = "ECDC niveau 'groen'", size=4,color = "black",face = "bold", hjust ="left")+
  annotate("curve", x = as.Date("2020-10-20"), xend =as.Date("2020-10-25"), 
           y = 600, yend = 375, curvature = -0.2,
           colour = "black", size=2, alpha=0.8, arrow =arrow(type = "closed",length = unit(2,"mm")))+
  
   ## prediction lines
  geom_line(data=df.daling.4, aes(x=Datum, y=r7), size = 1.25, color = "black")+
 #  geom_line(data=df.daling.4, aes(x=Datum, y=r8))+
 #  geom_line(data=df.daling.4, aes(x=Datum, y=r9))+
 ## geom_line(data=df.daling.4, aes(x=Datum, y=r10), size = 1.25, color = "darkgray")+
 ## geom_line(data=df.daling.4, aes(x=Datum, y=r12), size = 1.25, color = "darkgray")+
 ##geom_line(data=df.daling.4, aes(x=Datum, y=r14), size = 1.25, color = "darkgray")+
  # geom_line(data=df.daling.4, aes(x=Datum, y=r16))+
  geom_line(data=df.daling.4, aes(x=Datum, y=r18))+     #waakzaam met kerst
 # geom_line(data=df.daling.4, aes(x=Datum, y=r15))+
  #geom_line(data=df.daling.4, aes(x=Datum, y=r105))+    # ECDC groen met kerst
 # geom_line(data=df.daling.4, aes(x=Datum, y=r21), size = 1.25, color = "darkgray")+
  geom_line(data=df.daling.4, aes(x=Datum, y=r28), size = 1.25, color = "black")+
  

 
  geom_line(mapping = aes(x=fixedDate, y=MACases), color = "#F5F5F5",lwd = 4)+
  geom_line(mapping = aes(x=fixedDate, y=MACases), color = "#44546a",lwd = 3)+
  
 # geom_vline(xintercept = as.Date("2020-12-25"), linetype = "dashed", size = 1.5, color = "darkgreen")+
 
 
 # geom_vline(data=sint.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "black")+
  #geom_text(data=sint.df  , mapping=aes(x=date, y=6500, label=event), size=5, angle=90, vjust=-0.4, hjust=0)+
  
  #######
  #geom_vline(data=sint.persco.df,  mapping=aes(xintercept=date), linetype = "dotted", size = 1, color = "black")+
  #geom_text(data=sint.persco.df  , mapping=aes(x=date, y=6500, label=event), size=4, angle=90, vjust=-0.4, hjust=0, color= "black")+
  
#  geom_vline(data=kerst.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "darkgreen")+
 # geom_text(data=kerst.df  , mapping=aes(x=date, y=6500, label=event), size=8, angle=90, vjust=-0.4, hjust=0)+
  
#geom_vline(data=sint.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "black")+
 # geom_text(data=sint.df  , mapping=aes(x=date, y=10000, label=event), size=4, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  geom_vline(data=sint.persco.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "black")+
  geom_text(data=sint.persco.df  , mapping=aes(x=date, y=10000, label=event), size=5, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  geom_vline(data=kerst.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "darkgreen")+
  geom_text(data=kerst.df  , mapping=aes(x=date, y=10000, label=event), size=8, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
 
   #geom_text(data=thanks.df, mapping=aes(x=as.Date("2020-11-26"), y=3850, label=thanks), size=8, angle=90, vjust=-0.4, hjust=0)+
  
  
  geom_line(data=df.predict.kerst, aes(x=fixedDate, y=MACases), size = 1, color = "darkred")+
  geom_point(data=df.predict.kerst, aes(x=fixedDate, y=MACases), size = 2, color = "black")+
  geom_point(data=df.predict.kerst, aes(x=fixedDate, y=MACases), size = 1.5, color = "darkred")+
 # geom_line(data=df.predict.kerst, aes(x=Date, y=MACases_2), size = 0.6, color = "black")+ 
  geom_point(data=df.predict.kerst, aes(x=Date, y=MACases_2), size = 1.5, color = "#F5F5F5", alpha  =0.75)+ 
  geom_point(data=df.predict.kerst, aes(x=Date, y=MACases_2), size = 1, color = "#44546a", alpha  =0.75)+ 
  
  
  annotate("text", x = as.Date("2020-11-13"), y = 8300,  label = "Verwachting kabinet:\nhalvering elke 4 weken", size=5,angle=-57, color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-11-06"), y = 6000,  label = "halvering elke 7 dagen", size=5,angle=-80, color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-11-16"), y = 5000,  label = "Waakzaam tijdens kerst", size=5,angle=-55, color = "black",face = "bold", hjust ="left")+
  
  # annotate("text", x = as.Date("2020-11-16"), y = 5500,  label = paste( doublingdayZ.1.text, "elke",doublingdayZ.1.int, "dagen"), size=5,angle=-40, color = "black",face = "bold", hjust ="left")+
  # annotate("text", x = as.Date("2020-11-16"), y = 4000,  label = paste( doublingdayZ.text, "elke",doublingdayZ.int, "dagen"), size=5,angle=-50, color = "black",face = "bold", hjust ="left")+
  
  theme_classic()+
    xlab("")+
    ylab("")+
   
  #scale_y_continuous()+ #trans='log2')+
    #lims(x= c(NA, NA), y = c(16, NA))+
    #ylim(16, NA)+
 
  scale_y_continuous(limits = c(0, 11500),breaks = c(2500, 5000, 7500,10000), labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  coord_cartesian(expand = FALSE)+
  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%d %b"),
               limits = as.Date(c("2020-07-01", "2021-02-05")))+
  
  labs(title = "De 'kunnen we kerst vieren?' grafiek",
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
  
      
  ggsave("data/60_trendlines_cases.png",width=16, height = 9)


#### prediction plot ####

ggplot(Merged_data_short)+
  
  
  ### stoplicht
 # annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2021-02-05"), ymin =6250, ymax = Inf, color = "black",fill = "red", alpha = 0.6)+
 # annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2021-02-05"), ymin =3750, ymax = 6250, color = "black",fill = "orange", alpha = 0.5)+
 # annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2021-02-05"), ymin =1250, ymax = 3750, color = "black",fill = "yellow", alpha = 0.4)+
 # annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2021-02-05"), ymin =0, ymax = 1250, color = "black",fill = "green", alpha = 0.3)+ 
  
  ### ECDC kleur
  #annotate("rect", xmin = as.Date("2020-08-01"), xmax =as.Date("2021-02-05"), ymin =0, ymax = 315, color = "black",fill = "green", alpha = 0.3)+ 
  
  ### routekaart kleuren ###
   annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2021-02-05"), ymin =6250, ymax = Inf, color = "black",fill = "#68032F", alpha = 1)+
   annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2021-02-05"), ymin =3750, ymax = 6250, color = "black",fill = "#BC2166", alpha = 1)+
   annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2021-02-05"), ymin =1250, ymax = 3750, color = "black",fill = "#DB5C94", alpha = 1)+
  annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2021-02-05"), ymin =0, ymax = 1250, color = "black",fill = "#F291BC", alpha = 1)+ 
  
  # geom_point(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"))+
  
  annotate("text", x = as.Date("2020-07-05"), y = 7500, label = "Zeer Ernstig", size=10,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-07-05"), y = 5000, label = "Ernstig", size=10,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-07-05"), y = 2500, label = "Zorgelijk", size=10,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-07-05"), y = 600,  label = "Waakzaam", size=10,color = "black",face = "bold", hjust ="left")+
  
  annotate("text", x = as.Date("2020-07-02"), y = 6360, label = "250/100K/week", size=2,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-07-02"), y = 3860, label = "150/100K/week", size=2,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-07-02"), y = 1320, label = "50/100K/week", size=2,color = "black",face = "bold", hjust ="left")+
 # annotate("text", x = as.Date("2020-10-26"), y = 390, label = "25/100K/ 2 weken", size=2,color = "black",face = "bold", hjust ="left")+
  
  
  geom_bar(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"), fill = "gray", alpha = 0.35)+     #, color = "#96afde"
 # scale_fill_manual(values=c("gray"), alpha = 0.3)+
  

  ## prediction lines
 # geom_line(data=df.daling.4, aes(x=Datum, y=r7), size = 1.25, color = "black")+
  geom_line(data=df.daling.4, aes(x=Datum, y=r18))+     #waakzaam met kerst
  geom_line(data=df.daling.4, aes(x=Datum, y=r28), size = 1.25, color = "black")+
  

  ###  7day MA  
  geom_line(mapping = aes(x=fixedDate, y=MACases), color = "#F5F5F5",lwd = 3)+
  geom_line(mapping = aes(x=fixedDate, y=MACases), color = "#44546a",lwd = 2)+
  
  # geom_vline(xintercept = as.Date("2020-12-25"), linetype = "dashed", size = 1.5, color = "darkgreen")+
  
  
 # geom_vline(data=sint.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "black")+
 # geom_text(data=sint.df  , mapping=aes(x=date, y=10000, label=event), size=4, angle=-90, vjust=-0.4, hjust=0, color= "#F5F5F5")+
  
  geom_vline(data=sint.persco.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "black")+
  geom_text(data=sint.persco.df  , mapping=aes(x=date, y=10000, label=event), size=4, angle=-90, vjust=-0.4, hjust=0, color= "#F5F5F5")+
  
  geom_vline(data=kerst.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "darkgreen")+
  geom_text(data=kerst.df  , mapping=aes(x=date, y=10000, label=event), size=8, angle=-90, vjust=-0.4, hjust=0, color= "#F5F5F5")+
  
  
  
  
  #geom_text(data=thanks.df, mapping=aes(x=as.Date("2020-11-26"), y=3850, label=thanks), size=8, angle=90, vjust=-0.4, hjust=0)+
  
  
#  geom_line(data=df.predict.kerst, aes(x=fixedDate, y=MACases), size = 1, color = "darkred")+
#  geom_point(data=df.predict.kerst, aes(x=fixedDate, y=MACases), size = 2, color = "black")+
#  geom_point(data=df.predict.kerst, aes(x=fixedDate, y=MACases), size = 1.5, color = "darkred")+
 
#  geom_point(data=df.predict.kerst, aes(x=Date, y=MACases_2), size = 1.5, color = "#F5F5F5")+ 
 # geom_point(data=df.predict.kerst, aes(x=Date, y=MACases_2), size = 1, color = "#44546a")+ 
  
  
  annotate("text", x = as.Date("2020-11-13"), y = 8300,  label = "Verwachting Kabinet:\nhalvering elke 4 weken", size=5,angle=-57, color = "#F5F5F5",face = "bold", hjust ="left")+
#  annotate("text", x = as.Date("2020-11-06"), y = 6000,  label = "halvering elke 7 dagen", size=5,angle=-80, color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-11-16"), y = 5000,  label = "Waakzaam tijdens kerst", size=5,angle=-55, color = "#F5F5F5",face = "bold", hjust ="left")+
  
 
  theme_classic()+
  xlab("")+
  ylab("")+
  

  scale_y_continuous(limits = c(0, 11500),breaks = c(2500, 5000, 7500,10000), labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  coord_cartesian(expand = FALSE)+
  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%d %b"),
               limits = as.Date(c("2020-07-01", "2021-02-28")))+
  
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
  ggsave("data/60_routekaart.png",width=16, height = 9)
  














ggplot(Merged_data_short)+
  

### routekaart kleuren ###
annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2021-03-01"), ymin =6250, ymax = Inf, color = "black",fill = "#68032F", alpha = 1)+
  annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2021-03-01"), ymin =3750, ymax = 6250, color = "black",fill = "#BC2166", alpha = 1)+
  annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2021-03-01"), ymin =1250, ymax = 3750, color = "black",fill = "#DB5C94", alpha = 1)+
  annotate("rect", xmin = as.Date("2020-07-01"), xmax =as.Date("2021-03-01"), ymin =0, ymax = 1250, color = "black",fill = "#F291BC", alpha = 1)+ 
  
  # geom_point(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"))+
  
  annotate("text", x = as.Date("2020-07-05"), y = 7500, label = "Zeer Ernstig", size=10,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-07-05"), y = 5000, label = "Ernstig", size=10,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-07-05"), y = 2500, label = "Zorgelijk", size=10,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-07-05"), y = 600,  label = "Waakzaam", size=10,color = "black",face = "bold", hjust ="left")+
  
  annotate("text", x = as.Date("2020-07-02"), y = 6360, label = "250/100K/week", size=2,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-07-02"), y = 3860, label = "150/100K/week", size=2,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2020-07-02"), y = 1320, label = "50/100K/week", size=2,color = "black",face = "bold", hjust ="left")+
  # annotate("text", x = as.Date("2020-10-26"), y = 390, label = "25/100K/ 2 weken", size=2,color = "black",face = "bold", hjust ="left")+
  
  
  geom_bar(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"), fill = "gray", alpha = 0.35)+     #, color = "#96afde"
  # scale_fill_manual(values=c("gray"), alpha = 0.3)+
  
  
  ## prediction lines
  # geom_line(data=df.daling.4, aes(x=Datum, y=r7), size = 1.25, color = "black")+
  geom_line(data=df.daling.4, aes(x=Datum, y=r18))+     #waakzaam met kerst
  geom_line(data=df.daling.4, aes(x=Datum, y=r28), size = 1.25, color = "black")+
  
  
  ###  7day MA  
  geom_line(mapping = aes(x=fixedDate, y=MACases), color = "#F5F5F5",lwd = 3)+
  geom_line(mapping = aes(x=fixedDate, y=MACases), color = "#44546a",lwd = 2)+
  
  # geom_vline(xintercept = as.Date("2020-12-25"), linetype = "dashed", size = 1.5, color = "darkgreen")+
  
  
  # geom_vline(data=sint.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "black")+
  # geom_text(data=sint.df  , mapping=aes(x=date, y=10000, label=event), size=4, angle=-90, vjust=-0.4, hjust=0, color= "#F5F5F5")+
  
  geom_vline(data=sint.persco.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "black")+
  geom_text(data=sint.persco.df  , mapping=aes(x=date, y=10000, label=event), size=4, angle=-90, vjust=-0.4, hjust=0, color= "#F5F5F5")+
  
  geom_vline(data=kerst.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "darkgreen")+
  geom_text(data=kerst.df  , mapping=aes(x=date, y=10000, label=event), size=8, angle=-90, vjust=-0.4, hjust=0, color= "#F5F5F5")+
  
  # carnaval.df=data.frame(date=as.Date(c("2021-01-13", "2021-01-16")),event=c(carnaval, carnaval))

  geom_vline(data=carnaval.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1.5, color = "red")+
  geom_text(data=carnaval.df  , mapping=aes(x=date, y=10000, label=event), size=8, angle=-90, vjust=-0.4, hjust=0, color= "#F5F5F5")+
  
  
  #geom_text(data=thanks.df, mapping=aes(x=as.Date("2020-11-26"), y=3850, label=thanks), size=8, angle=90, vjust=-0.4, hjust=0)+
  
  
  #  geom_line(data=df.predict.kerst, aes(x=fixedDate, y=MACases), size = 1, color = "darkred")+
  #  geom_point(data=df.predict.kerst, aes(x=fixedDate, y=MACases), size = 2, color = "black")+
  #  geom_point(data=df.predict.kerst, aes(x=fixedDate, y=MACases), size = 1.5, color = "darkred")+
  
#  geom_point(data=df.predict.kerst, aes(x=Date, y=MACases_2), size = 1.5, color = "#F5F5F5")+ 
# geom_point(data=df.predict.kerst, aes(x=Date, y=MACases_2), size = 1, color = "#44546a")+ 


annotate("text", x = as.Date("2020-11-13"), y = 8300,  label = "Verwachting Kabinet:\nhalvering elke 4 weken", size=5,angle=-57, color = "#F5F5F5",face = "bold", hjust ="left")+
annotate("text", x = as.Date("2020-11-16"), y = 5000,  label = "Waakzaam tijdens kerst", size=5,angle=-55, color = "#F5F5F5",face = "bold", hjust ="left")+
  
  
  theme_classic()+
  xlab("")+
  ylab("")+
  
  
  scale_y_continuous(limits = c(0, 11500),breaks = c(2500, 5000, 7500,10000), labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  coord_cartesian(expand = FALSE)+
  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%d %b"),
               limits = as.Date(c("2020-07-01", "2021-03-01")))+
  
  labs(title = "Routekaartkleuren",
       subtitle = "met 7-daags voortschrijdend gemiddelde",
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
    panel.grid.major.x = element_line(colour= "lightgray", linetype = "dashed")
  )+ 
  
 
  ggsave("data/60_routekaart_carnaval.png",width=16, height = 9)

