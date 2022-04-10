
today = Sys.Date()

lcps.capacity.hist.file <-    paste("C:\\Rdir\\data\\",Sys.Date()-1,"\\", Sys.Date()-1, "_lcps_ic-_capacity.csv", sep = "")
lcps.capacity.hist<- read.csv(lcps.capacity.hist.file,sep=",")

colnames(lcps.capacity.hist) <- c("date", "ic_capacity")
lcps.capacity.hist$date <- as.Date(lcps.capacity.hist$date)
lcps.capacity.hist[nrow(lcps.capacity.hist) + 1,] = data.frame(today, beds_current_capacity)

lcps.capacity.hist.file2 <-    paste("C:\\Rdir\\data\\",Sys.Date(),"\\", Sys.Date(), "_lcps_ic-_capacity.csv", sep = "")
write.csv(lcps.capacity.hist, file = lcps.capacity.hist.file2, row.names = F)


IC_occupation3  <- IC_occupation
IC_occupation3$boss <-188

beds_total       = 188 + beds_covid  +  beds_non_covid
beds_needed      = beds_total-beds_current_capacity
boss_open      = 188 - beds_needed


if (beds_needed < 0) {
  beds.needed.dot <- intToUtf8(0x1F7E2)     ### groen
  tekort.label <- paste("BOSS tekort:           ---")
  beds_needed.neg = -beds_needed
  beds_needed.tweet     <- paste(beds.needed.dot, "Geen IC-bedden tekort. Vrije bedden: ", beds_needed.neg)
  
  
} else if (beds_needed > 0) {
  beds.needed.dot <- intToUtf8(0x1F534)     ### rood
  tekort.label <- paste("BOSS tekort:           ", beds_needed)
  beds_needed.tweet     <- paste(beds.needed.dot, beds_needed, "IC-bedden tekort (gaat af van de BOSS bedden, die eigenlijk vrij moeten blijven)")
  
  
} else if (beds_needed == 0)
  {beds.needed.dot <- intToUtf8(0x1F7E1)   ### geel
  tekort.label <- paste("BOSS tekort:           ---")
beds_needed.tweet     <- paste(beds.needed.dot, beds_needed, "geen IC-bedden tekort.")}

  

  
  
if (boss_open < 188) {
    hosp.total.dot <- intToUtf8(0x1F7E2)     ### groen
    boss.open    <- paste("BOSS open:           ", boss_open)
    
  } else if (boss_open > 188) {
    hosp.total.dot <- intToUtf8(0x1F534)     ### rood
    boss.open    <- paste("BOSS open:           188")
    
  } else
    {hosp.total.dot <- intToUtf8(0x1F7E1)     ### geel
  boss.open    <- paste("BOSS open:           ", 188)}


IC_occupation3 <- merge(IC_occupation3,lcps.capacity.hist, all.x=TRUE)
IC_occupation3$needed <- (IC_occupation3$boss+IC_occupation3$B_IC_covid_nl+IC_occupation3$A_IC_neg)-IC_occupation3$ic_capacity


IC_occupation3$needed[IC_occupation3$needed < 0] <- 0    ### make sure beds are not double counted


IC_occupation3$boss_minus = IC_occupation3$boss-IC_occupation3$needed
IC_occupation3$over <- IC_occupation3$ic_capacity - (IC_occupation3$boss+IC_occupation3$B_IC_covid_nl+IC_occupation3$A_IC_neg)



keycol <- "date"
valuecol <- "type"
gathercols <- c("B_IC_covid_nl","A_IC_neg","boss_minus","needed", "over")


lcps_working_4_long <- gather(IC_occupation3, keycol, valuecol, gathercols)



colnames(lcps_working_4_long) <- c("date","ic_capacity", "boss", "type","number")




#### zoom ####
ggplot(data = lcps_working_4_long,
       mapping = aes(x = date, y = number, color = factor(type, levels=c("over", "needed", "boss_minus","A_IC_neg", "B_IC_covid_nl")), 
                     fill = factor(type, levels=c("over","needed", "boss_minus","A_IC_neg", "B_IC_covid_nl"))))+
  
  annotate("rect", xmin = as.Date("2021-08-12"), xmax =as.Date("2022-07-28"), ymin =1350, ymax = 1500, color = "black",fill = "black", alpha = 0.9)+ 
  annotate("rect", xmin = as.Date("2021-08-12"), xmax =as.Date("2022-07-28"), ymin =1150, ymax = 1350, color = "black",fill = "black", alpha = 0.3)+ 
  annotate("rect", xmin = as.Date("2021-08-12"), xmax =as.Date("2022-07-28"), ymin =950, ymax = 1150, color = "black",fill = "red", alpha = 0.5)+ 
  annotate("rect", xmin = as.Date("2021-08-12"), xmax =as.Date("2022-07-28"), ymin =400, ymax = 950, color = "black",fill = "blue", alpha = 0.3)+ 
  annotate("rect", xmin = as.Date("2021-08-12"), xmax =as.Date("2022-07-28"), ymin =200, ymax = 400, color = "black",fill = "red", alpha = 0.3)+ 
  annotate("rect", xmin = as.Date("2021-08-12"), xmax =as.Date("2022-07-28"), ymin =100, ymax = 200, color = "black",fill = "yellow", alpha = 0.3)+ 
  annotate("rect", xmin = as.Date("2021-08-12"), xmax =as.Date("2022-07-28"), ymin =0, ymax = 100, color = "black",fill = "blue", alpha = 0.3)+ 

  
  geom_text( aes( x=as.Date("2022-07-03"), y=1450, label=" CODE BLACK"),
             color="white", size=4 , angle=0, fontface="bold")+ 
  
  geom_text( aes( x=as.Date("2022-07-03"), y=1300, label="1.350 - Hugo's\nopschalingsdoel"),
             color="black", size=4 , angle=0, fontface="bold")+ 
  
  geom_text( aes( x=as.Date("2022-07-03"), y=1130, label="1.150 - IC max (piek) "),
             color="black", size=4 , angle=0, fontface="bold")+ 
  
  geom_text( aes( x=as.Date("2022-07-03"), y=930, label="950 - IC max (langdurig)"),
             color="black", size=4 , angle=0, fontface="bold")+ 
  
  geom_text( aes( x=as.Date("2022-06-25"), y=380, label="400 - max COVID-19 (zonder griep)"),
             color="black", size=4 , angle=0, fontface="bold")+
  
  geom_text( aes( x=as.Date("2022-06-25"), y=180, label="200 - max COVID-19 (met griep)"),
             color="black", size=4 , angle=0, fontface="bold")+
  
  geom_text( aes( x=as.Date("2022-06-25"), y=50, label="Gommers goede zorg doel:\n max 100 covid op IC"),
             color="black", size=4 , angle=0, fontface="bold")+
  
  geom_bar(stat='identity')+
  
  scale_x_date(name="",
               date_breaks = "1 month",
               date_labels= format("%b"),
               limits = as.Date(c("2021-08-12", NA)))+
  ylab("")+
  scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","),
                      breaks=c(100, 200,400,750,950,1150,1350),
                    #  sec.axis = dup_axis(),
                      limits = c(0,1500)
  )+
  
  scale_fill_manual(values  =c("#04de42", "darkred", "darkgray", "#C5E0B4", "#4472C4"), labels=c("Overcapaciteit", tekort.label, boss.open, hosp_clin.a, hosp_IC.a))+
  scale_color_manual(values =c("black", "black",   "black"  ,  "#767171", "#3B3838"), labels=c("Overcapaciteit", tekort.label, boss.open, hosp_clin.a, hosp_IC.a))+
  
  labs(title=hosp_title.boss,
       caption = paste("Source: LCPS | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  
  theme(legend.position = c(0.3, 0.9),
        legend.background = element_rect(fill="#DAE3F3",size=0.8,linetype="solid",colour ="black"),
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
          panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))+
  
  ggsave("data/plots/16b_IC_only_zoom.png",width=16, height = 9)







####  IC zoom tweet #####          

my_timeline <- get_my_timeline()  
reply_id <- my_timeline$status_id[1]


tweet.zoom.tweet <- 
  
  "Grens aan de zorg.

Benodigde capaciteit:
- 188 BOSS bedden: Beds Open for Safety and Support
- %s non-COVID bedden
- %s COVID bedden

%s bedden nodig.
%s beschikbaar 

%s"


tweet.zoom.tweet <- sprintf(tweet.zoom.tweet,
                            beds_non_covid,
                            beds_covid,
                            beds_total,
                            beds_current_capacity,
                            beds_needed.tweet)
Encoding(tweet.zoom.tweet) <- "UTF-8"

post_tweet(tweet.zoom.tweet,  media = c("data/plots/16b_IC_only_zoom.png" 
), in_reply_to_status_id = reply_id)

