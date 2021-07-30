
today = Sys.Date()
as.Date(today)
easing <- as.Date("2021-06-26")




casus.age.dif.play <- cassus.age.spread.base  # read.csv("C:\\Rdir\\data-contstant\\casus_daily_age_dif.csv" ,sep=";", check.names = FALSE)

casus.age.dif.play$date <- as.Date(casus.age.dif.play$date)
casus.age.dif.play$`0-9`     <- as.integer(casus.age.dif.play$`0-9` )
casus.age.dif.play$`10-19`   <- as.integer(casus.age.dif.play$`10-19`)
casus.age.dif.play$`20-29`   <- as.integer(casus.age.dif.play$`20-29` )
casus.age.dif.play$`30-39`   <- as.integer(casus.age.dif.play$`30-39` )
casus.age.dif.play$`40-49`   <- as.integer(casus.age.dif.play$`40-49`)
casus.age.dif.play$`50-59`   <- as.integer(casus.age.dif.play$`50-59`)
casus.age.dif.play$`60-69`   <- as.integer(casus.age.dif.play$`60-69`)
casus.age.dif.play$`70-79`   <- as.integer(casus.age.dif.play$`70-79`)
casus.age.dif.play$`80-89`   <- as.integer(casus.age.dif.play$`80-89`)
casus.age.dif.play$`90+`    <- as.integer(casus.age.dif.play$`90+`)


casus.age.dif.play$`MA.0-9`    <- rollmeanr((casus.age.dif.play$`0-9` *100000/(1755525))   , 7, fill = 0)
casus.age.dif.play$`MA.10-19`  <- rollmeanr((casus.age.dif.play$`10-19`*100000/(1983199))  , 7, fill = 0)
casus.age.dif.play$`MA.20-29`  <- rollmeanr((casus.age.dif.play$`20-29`*100000/(2240325))  , 7, fill = 0)
casus.age.dif.play$`MA.30-39`  <- rollmeanr((casus.age.dif.play$`30-39`*100000/(2184528))  , 7, fill = 0)
casus.age.dif.play$`MA.40-49`  <- rollmeanr((casus.age.dif.play$`40-49`*100000/(2162516))  , 7, fill = 0)
casus.age.dif.play$`MA.50-59`  <- rollmeanr((casus.age.dif.play$`50-59`*100000/(2550020))  , 7, fill = 0)
casus.age.dif.play$`MA.60-69`  <- rollmeanr((casus.age.dif.play$`60-69`*100000/(2143755))  , 7, fill = 0)
casus.age.dif.play$`MA.70-79`  <- rollmeanr((casus.age.dif.play$`70-79`*100000/(1618615))  , 7, fill = 0)
casus.age.dif.play$`MA.80-89`  <- rollmeanr((casus.age.dif.play$`80-89`*100000/(706413))  , 7, fill = 0)
casus.age.dif.play$`MA.90+`    <- rollmeanr((casus.age.dif.play$`90+`  *100000/(131012))  , 7, fill = 0)



#relative.casus.age.dif.play <- casus.age.dif.play[ -c(1:11,13:23)]
casus.age.dif.play <- casus.age.dif.play[casus.age.dif.play$date > "2021-05-26",]  #"2021-06-08",]
casus.age.dif.play.short <- casus.age.dif.play[casus.age.dif.play$date >"2021-02-16",]





hosp.risk <- casus.age.dif.play


hosp.risk <- hosp.risk[,-(13:23)]   

last.day.hosp.risk <- last(hosp.risk)
last.week.hosp.risk <- last(hosp.risk,7)

days.from.easing = as.numeric(difftime(as.POSIXct(today), as.POSIXct(easing, tz="UTC"), units="days"))

last.26.hosp.risk <- last(hosp.risk,days.from.easing)
last.26.hosp.risk <- last.26.hosp.risk [,-(1)]  
last.26.hosp.risk.sum <- last.26.hosp.risk [,-(11)]  


total.cases.delta <- sum(last.26.hosp.risk.sum)
total.cases.delta

File_date_onv <- paste0("data/opnames_verwacht_", format(Sys.time(), "%Y-%m-%d"),".csv")
write.csv(last.26.hosp.risk, File_date_onv, row.names=FALSE) 

last.26.hosp.risk.working <- last.26.hosp.risk
last.26.hosp.risk.working$`20-` <- last.26.hosp.risk.working$`0-9` + last.26.hosp.risk.working$`10-19`

last.26.hosp.risk.working$`20-CStd` <- last.26.hosp.risk.working$`20-`     * 0.001350044
last.26.hosp.risk.working$`20-29CStd` <- last.26.hosp.risk.working$`20-29` * 0.002003394
last.26.hosp.risk.working$`30-39CStd` <- last.26.hosp.risk.working$`30-39` * 0.006444603
last.26.hosp.risk.working$`40-49CStd` <- last.26.hosp.risk.working$`40-49` * 0.009787346
last.26.hosp.risk.working$`50-59CStd` <- last.26.hosp.risk.working$`50-59` * 0.025945658
last.26.hosp.risk.working$`60-69CStd` <- last.26.hosp.risk.working$`60-69` * 0.038038507
last.26.hosp.risk.working$`70-79CStd` <- last.26.hosp.risk.working$`70-79` * 0.102677075
last.26.hosp.risk.working$`80-89CStd` <- last.26.hosp.risk.working$`80-89` * 0.169386085
last.26.hosp.risk.working$`90+CStd` <- last.26.hosp.risk.working$`90+`     * 0.101264418

last.26.hosp.risk.working$sumSanq <- last.26.hosp.risk.working$`20-CStd`+ 
  last.26.hosp.risk.working$`20-29CStd`+
  last.26.hosp.risk.working$`30-39CStd` + 
  last.26.hosp.risk.working$`40-49CStd` + 
  last.26.hosp.risk.working$`50-59CStd` + 
  last.26.hosp.risk.working$`60-69CStd` + 
  last.26.hosp.risk.working$`70-79CStd` + 
  last.26.hosp.risk.working$`80-89CStd` + 
  last.26.hosp.risk.working$`90+CStd`  


last.26.hosp.risk.working$`20-CDelta` <- last.26.hosp.risk.working$`20-`     * 0.004479733
last.26.hosp.risk.working$`20-29CDelta` <- last.26.hosp.risk.working$`20-29` * 0.003852120
last.26.hosp.risk.working$`30-39CDelta` <- last.26.hosp.risk.working$`30-39` * 0.010023150
last.26.hosp.risk.working$`40-49CDelta` <- last.26.hosp.risk.working$`40-49` * 0.017792271
last.26.hosp.risk.working$`50-59CDelta` <- last.26.hosp.risk.working$`50-59` * 0.035158270
last.26.hosp.risk.working$`60-69CDelta` <- last.26.hosp.risk.working$`60-69` * 0.071176242
last.26.hosp.risk.working$`70-79CDelta` <- last.26.hosp.risk.working$`70-79` * 0.160921640
last.26.hosp.risk.working$`80-89CDelta` <- last.26.hosp.risk.working$`80-89` * 0.196822208
last.26.hosp.risk.working$`90+CDelta` <- last.26.hosp.risk.working$`90+`     * 0.101264418

last.26.hosp.risk.working$sumRIVM <- last.26.hosp.risk.working$`20-CDelta`+ 
  last.26.hosp.risk.working$`20-29CDelta`+
  last.26.hosp.risk.working$`30-39CDelta` + 
  last.26.hosp.risk.working$`40-49CDelta` + 
  last.26.hosp.risk.working$`50-59CDelta` + 
  last.26.hosp.risk.working$`60-69CDelta` + 
  last.26.hosp.risk.working$`70-79CDelta` + 
  last.26.hosp.risk.working$`80-89CDelta` + 
  last.26.hosp.risk.working$`90+CDelta`  

 last.26.hosp.risk.working$`20-CStdDelta` <- last.26.hosp.risk.working$`20-`     * 0.001350044 * 1.8
 last.26.hosp.risk.working$`20-29CStdDelta` <- last.26.hosp.risk.working$`20-29` * 0.002003394 * 1.8
 last.26.hosp.risk.working$`30-39CStdDelta` <- last.26.hosp.risk.working$`30-39` * 0.006444603 * 1.8
 last.26.hosp.risk.working$`40-49CStdDelta` <- last.26.hosp.risk.working$`40-49` * 0.009787346 * 1.8
 last.26.hosp.risk.working$`50-59CStdDelta` <- last.26.hosp.risk.working$`50-59` * 0.025945658 * 1.8
 last.26.hosp.risk.working$`60-69CStdDelta` <- last.26.hosp.risk.working$`60-69` * 0.038038507 * 1.8
 last.26.hosp.risk.working$`70-79CStdDelta` <- last.26.hosp.risk.working$`70-79` * 0.102677075 * 1.8
 last.26.hosp.risk.working$`80-89CStdDelta` <- last.26.hosp.risk.working$`80-89` * 0.169386085 * 1.8
 last.26.hosp.risk.working$`90+CStdDelta` <- last.26.hosp.risk.working$`90+`     * 0.101264418 * 1.8

 last.26.hosp.risk.working$sumSanqDelta <- last.26.hosp.risk.working$`20-CStdDelta`+ 
   last.26.hosp.risk.working$`20-29CStdDelta`+
   last.26.hosp.risk.working$`30-39CStdDelta` + 
   last.26.hosp.risk.working$`40-49CStdDelta` + 
   last.26.hosp.risk.working$`50-59CStdDelta` + 
   last.26.hosp.risk.working$`60-69CStdDelta` + 
   last.26.hosp.risk.working$`70-79CStdDelta` + 
   last.26.hosp.risk.working$`80-89CStdDelta` + 
   last.26.hosp.risk.working$`90+CStdDelta`  

 
 last.26.hosp.risk.working$`20-RivmDelta` <- last.26.hosp.risk.working$`20-`     * (0.001350044 * 1.8) * 1
 last.26.hosp.risk.working$`20-29RivmDelta` <- last.26.hosp.risk.working$`20-29` * (0.002003394 * 1.8) * 0.97
 last.26.hosp.risk.working$`30-39RivmDelta` <- last.26.hosp.risk.working$`30-39` * (0.006444603 * 1.8) * 0.92
 last.26.hosp.risk.working$`40-49RivmDelta` <- last.26.hosp.risk.working$`40-49` * (0.009787346 * 1.8) * 0.89
 last.26.hosp.risk.working$`50-59RivmDelta` <- last.26.hosp.risk.working$`50-59` * (0.025945658 * 1.8) * 0.71
 last.26.hosp.risk.working$`60-69RivmDelta` <- last.26.hosp.risk.working$`60-69` * (0.038038507 * 1.8) * 0.58
 last.26.hosp.risk.working$`70-79RivmDelta` <- last.26.hosp.risk.working$`70-79` * (0.102677075 * 1.8) * 0.24
 last.26.hosp.risk.working$`80-89RivmDelta` <- last.26.hosp.risk.working$`80-89` * (0.169386085 * 1.8) * 0.22 
 last.26.hosp.risk.working$`90+RivmDelta` <- last.26.hosp.risk.working$`90+`     * (0.101264418 * 1.8) * 0.22 
 
 last.26.hosp.risk.working$sumSanqDeltaVac <- last.26.hosp.risk.working$`20-RivmDelta`+ 
   last.26.hosp.risk.working$`20-29RivmDelta`+
   last.26.hosp.risk.working$`30-39RivmDelta` + 
   last.26.hosp.risk.working$`40-49RivmDelta` + 
   last.26.hosp.risk.working$`50-59RivmDelta` + 
   last.26.hosp.risk.working$`60-69RivmDelta` + 
   last.26.hosp.risk.working$`70-79RivmDelta` + 
   last.26.hosp.risk.working$`80-89RivmDelta` + 
   last.26.hosp.risk.working$`90+RivmDelta`  
 
 
 
 
 #### no vac  #####
 
 last.26.hosp.risk.working$sumRaw <- last.26.hosp.risk.working$`20-`+ 
   last.26.hosp.risk.working$`20-29`+
   last.26.hosp.risk.working$`30-39` + 
   last.26.hosp.risk.working$`40-49` + 
   last.26.hosp.risk.working$`50-59` + 
   last.26.hosp.risk.working$`60-69` + 
   last.26.hosp.risk.working$`70-79` + 
   last.26.hosp.risk.working$`80-89` + 
   last.26.hosp.risk.working$`90+`  
 
 
 							
 
 
 
 last.26.hosp.risk.working$`20-NoVac`   <- last.26.hosp.risk.working$sumRaw * (0.001350044 * 1.8) * 0.127841346
 last.26.hosp.risk.working$`20-29NoVac` <- last.26.hosp.risk.working$sumRaw * (0.002003394 * 1.8) * 0.168347885
 last.26.hosp.risk.working$`30-39NoVac` <- last.26.hosp.risk.working$sumRaw * (0.006444603 * 1.8) * 0.135446605
 last.26.hosp.risk.working$`40-49NoVac` <- last.26.hosp.risk.working$sumRaw * (0.009787346 * 1.8) * 0.13297328
 last.26.hosp.risk.working$`50-59NoVac` <- last.26.hosp.risk.working$sumRaw * (0.025945658 * 1.8) * 0.188014458
 last.26.hosp.risk.working$`60-69NoVac` <- last.26.hosp.risk.working$sumRaw * (0.038038507 * 1.8) * 0.113384869
 last.26.hosp.risk.working$`70-79NoVac` <- last.26.hosp.risk.working$sumRaw * (0.102677075 * 1.8) *	0.067886973 
 last.26.hosp.risk.working$`80-89NoVac` <- last.26.hosp.risk.working$sumRaw * (0.169386085 * 1.8) * 0.048856743
 last.26.hosp.risk.working$`90+NoVac`   <- last.26.hosp.risk.working$sumRaw * (0.101264418 * 1.8) * 0.017247841
 
 last.26.hosp.risk.working$sumNoVac <- last.26.hosp.risk.working$`20-NoVac`+ 
   last.26.hosp.risk.working$`20-29NoVac`+
   last.26.hosp.risk.working$`30-39NoVac` + 
   last.26.hosp.risk.working$`40-49NoVac` + 
   last.26.hosp.risk.working$`50-59NoVac` + 
   last.26.hosp.risk.working$`60-69NoVac` + 
   last.26.hosp.risk.working$`70-79NoVac` + 
   last.26.hosp.risk.working$`80-89NoVac` + 
   last.26.hosp.risk.working$`90+NoVac`  
 
 last.26.hosp.risk.working$sumNoVacDelta <- last.26.hosp.risk.working$sumNoVac
 
 last.26.hosp.risk.working$sumNoVac <- last.26.hosp.risk.working$sumNoVac / 1.8
 
 last.26.hosp.risk.working$sumRawFactor <- last.26.hosp.risk.working$sumRaw /100 *1.5
 
#last.26.hosp.risk.sum.Std <- last.26.hosp.risk.working[,-(1:10)]
#last.26.hosp.risk.sum.Std <- last.26.hosp.risk.sum.Std[,-(2:11)]
#last.26.hosp.risk.sum.Std <- last.26.hosp.risk.sum.Std[,-(3:11)]

#last.26.hosp.risk.sum.Std$date.delay21 <- last.26.hosp.risk.sum.Std$date+21
#last.26.hosp.risk.sum.Std$date.delay12 <- last.26.hosp.risk.sum.Std$date+12
#last.26.hosp.risk.sum.Std$date.delay7 <- last.26.hosp.risk.sum.Std$date+7
#last.26.hosp.risk.sum.Std$date.delay21 <- last.26.hosp.risk.sum.Std$date+21
#last.26.hosp.risk.sum.Std$date.delay12 <- last.26.hosp.risk.sum.Std$date+12
#last.26.hosp.risk.sum.Std$date.delay7 <- last.26.hosp.risk.sum.Std$date+7

# last.26.hosp.risk.working$sumVaceffect.delay7 <- last.26.hosp.risk.sum.Std$date+7

last.26.hosp.risk.working$date.delay7 <- last.26.hosp.risk.working$date+7

last.26.hosp.risk.working.age <- last.26.hosp.risk.working

last.26.hosp.risk.working <- last.26.hosp.risk.working[,-(12:21)]
last.26.hosp.risk.working <- last.26.hosp.risk.working[,-(13:21)]
last.26.hosp.risk.working <- last.26.hosp.risk.working[,-(14:22)]
last.26.hosp.risk.working <- last.26.hosp.risk.working[,-(15:23)]


ggplot(LCPS_datafeed_predict)+
  
  
  geom_rect( aes(xmin = as.Date(today)-0.5,
                 xmax = as.Date(today)+0.5,
                 ymin = 0,
                 ymax = Inf,
  ), fill = "gray", alpha = 0.025)+
  
 # geom_point(data=last.26.hosp.risk.working, aes(x=date.delay7, y = sum, color = 'red'))+
  #geom_smooth(data=last.26.hosp.risk.sum.Std, aes(x=date.delay12, y = sum, color = 'red'))+
  geom_col(position = "dodge",  aes(x=Datum, y=Kliniek_Nieuwe_Opnames_COVID ), fill ="#c47945")+       #"#F4B183")+  
 
  
    
 # geom_point(data=last.26.hosp.risk.working, aes(x=date.delay7, y = sumRIVM, color = 'blue'))+
 
#  geom_smooth(data=last.26.hosp.risk.sum.Std, aes(x=date.delay12, y = sumDelta, color = 'blue'))+

  geom_smooth(data=last.26.hosp.risk.working, aes(x=date.delay7, y = sumSanq, color = 'sumSanq'))+
  geom_smooth(data=last.26.hosp.risk.working, aes(x=date.delay7, y = sumRIVM, color = 'sumRIVM'))+
  geom_smooth(data=last.26.hosp.risk.working, aes(x=date.delay7, y = sumSanqDelta, color = 'sumSanqDelta'))+   

  geom_smooth(data=last.26.hosp.risk.working, aes(x=date.delay7, y = sumSanqDeltaVac, color = 'sumSanqDeltaVac'))+
  geom_point(data=last.26.hosp.risk.working, aes(x=date.delay7, y = sumSanqDeltaVac, color = 'sumSanqDeltaVac'))+   
  
  
  geom_smooth(data=last.26.hosp.risk.working, aes(x=date.delay7, y = sumNoVac, color = 'sumNoVac'))+
  geom_smooth(data=last.26.hosp.risk.working, aes(x=date.delay7, y = sumNoVacDelta, color = 'sumNoVacDelta'))+
  
  geom_smooth(data=last.26.hosp.risk.working, aes(x=date.delay7, y = sumRawFactor, color = 'sumRawFactor'))+
  
  

    
  #scale_color_manual(values=c("blue", "red", "black", "darkgreen"), labels=c("RIVM", "Sanquin (#bierviltje)", "sanq+delta+vacc", "Sanq.Delta" ))+
    
    
#    "#bierviltje +\n Deltafactor (1.8x)","#bierviltje"))+
  
  
  
  
    geom_line(data = LCPS_datafeed_predict, aes(x=Datum, y=MA_clin_lead), size =3, color = "#DAE3F3")+
  geom_line(data = LCPS_datafeed_predict, aes(x=Datum, y=MA_clin_lead), size =2)+
 
   geom_vline(data=stap.drie.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
  geom_text(data=stap.drie.df  , mapping=aes(x=date, y=195, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  geom_vline(data=stap.vier.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
  geom_text(data=stap.vier.df  , mapping=aes(x=date, y=195, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  geom_vline(data=stap.zes.df,  mapping=aes(xintercept=date), linetype = "dashed", size = 1, color = "black")+
  geom_text(data=stap.zes.df  , mapping=aes(x=date, y=195, label=event), size=6, angle=-90, vjust=-0.4, hjust=0, color= "black")+
  
  

  
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%d %b"),
               name="",
               #               limits = as.Date(c("2020-10-18", NA)))+
               limits = as.Date(c("2021-05-20", "2021-08-05")))+
  
  #  scale_y_continuous(limits = c(0, NA), labels = label_comma(big.mark = ".", decimal.mark = ","), breaks = c(0,12,40,80,100,200,300,400))+
  scale_y_continuous(limits = c(0, 175), labels = label_comma(big.mark = ".", decimal.mark = ","))+ # , breaks = c(0,25,50,75,100,125,150,175,200))+

coord_cartesian(expand = FALSE)+
  
  ylab("")+
  labs(title="Yorick doet GEEN voorspelling grafiek", 
       subtitle = "Met time delay van: 7 dagen na melding",
       caption = paste("Bron: LCPS | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  
  theme(  plot.background = element_rect(fill = "#DAE3F3"),
          plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
          plot.subtitle = element_text(hjust = 0.5,size = 15,face = "italic"),
          panel.background = element_rect(fill = "#DAE3F3", colour = "#DAE3F3"),
          axis.text = element_text(size=14,color = "black",face = "bold"),
          axis.text.y = element_text(face="bold", color="black", size=14),  
          axis.ticks = element_line(colour = "#DAE3F3", size = 1, linetype = "solid"),
          axis.ticks.length = unit(0.5, "cm"),
          axis.line = element_line(colour = "#DAE3F3"),
          panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))+
  
  theme(#legend.position = c(0.5, 0.8),
    legend.background = element_rect(fill="#DAE3F3",size=0.8,linetype="solid",colour ="black"),
    legend.title = element_blank(),
    legend.text = element_text(colour="black", size=10, face="bold"))+
  
  
  ggsave("data/plots/16x_hosp_pred_summer.png",width=16, height = 9)







