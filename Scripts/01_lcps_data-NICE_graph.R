today = Sys.Date()

# New patients at IC 
ic_intake <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/new-intake/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill=TRUE)

ic_intake <- as.data.frame(t(ic_intake[c(1,2,4),]))

ic_intake$date <- unlist(ic_intake$V1)
ic_intake$ic_intake_proven <- unlist(ic_intake$V2)
ic_intake$ic_intake_suspected <- unlist(ic_intake$V3)
ic_intake <- ic_intake[,c(4:6)]

ic_intake$sum = ic_intake$ic_intake_proven+ic_intake$ic_intake_suspected
ic_intake <- ic_intake %>% 
  mutate(IC_MA7 = rollapply(sum, 7, mean, fill = NA, align = "right"))
ic_intake$IC_MA7 = round(ic_intake$IC_MA7, 1)


ic_intake <- ic_intake[ic_intake$date>"2021-01-01"&ic_intake$date<=today-3,]

# Intake per day of patients in hospital (non-IC) with suspected and/or proven covid-19
json_zkh_df <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/new-intake/",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill=TRUE)

zkh_new <- as.data.frame(t(json_zkh_df[c(1,2,4),]))

zkh_new$date <- unlist(zkh_new$V1)
zkh_new$new_hosp_proven <- unlist(zkh_new$V2)
zkh_new$new_hosp_suspected <- unlist(zkh_new$V3)
zkh_new <- zkh_new[,c(4:6)]

zkh_new$sum = zkh_new$new_hosp_proven+zkh_new$new_hosp_suspected
zkh_new <- zkh_new %>% 
  mutate(zkh_MA7 = rollapply(sum, 7, mean, fill = NA, align = "right"))
zkh_new$zkh_MA7 = round(zkh_new$zkh_MA7, 1)

zkh_new <- zkh_new[zkh_new$date>"2021-01-01"&zkh_new$date<=today-3,]

instroom.combi = merge(zkh_new,ic_intake, by="date")
instroom.combi <- instroom.combi[,c(1,5,9)]


instroom.combi$zkh_MA7_eight  <- lead(instroom.combi$zkh_MA7,7)
instroom.combi$IC_MA7_eight  <- lead(instroom.combi$IC_MA7,7)

instroom.combi$zkh_percentage <- instroom.combi$zkh_MA7_eight/instroom.combi$zkh_MA7
instroom.combi$zkh_percentage <- lag(instroom.combi$zkh_percentage,7)
instroom.combi$zkh_peak <- instroom.combi$zkh_MA7/257.4

instroom.combi$ic_percentage <- instroom.combi$IC_MA7_eight/instroom.combi$IC_MA7
instroom.combi$ic_percentage <- lag(instroom.combi$ic_percentage,7)
instroom.combi$ic_peak <- instroom.combi$IC_MA7/57.3



instroom.combi$date <- as.Date(instroom.combi$date)


instroom.lcps <- LCPS_datafeed_predict
instroom.lcps$peak_zkh_lcsp <- lag(instroom.lcps$peak,7)
instroom.lcps$peak_IC_lcsp <- lag(instroom.lcps$peak.IC,7)

instroom.lcps <- instroom.lcps[,c(1,19,20)]
instroom.lcps <- instroom.lcps[instroom.lcps$Datum>"2021-01-01"&instroom.lcps$Datum<=today,]
colnames(instroom.lcps) <- c("date", "peak_zkh_lcsp", "peak_IC_lcsp")


instoom.combi.big <- merge(instroom.lcps,instroom.combi, by= 'date', all.x = TRUE)


key <- "date"
value <- "percentage"
gathercols <- c("zkh_peak","ic_peak", "peak_zkh_lcsp", "peak_IC_lcsp")
instroom.combi.long1 <- gather(instoom.combi.big, key, value, gathercols) #(2:11))

instroom.combi.long1$date = as.Date(instroom.combi.long1$date)




ggplot(instroom.combi.long1, aes(x=date, y=value, color = factor(key, levels=c("zkh_peak","peak_zkh_lcsp","ic_peak","peak_IC_lcsp"))))+
  
  geom_hline(yintercept=1, size = 1, linetype = "dashed")+
  geom_hline(yintercept=0.80, color = "darkgreen", size = 2, linetype = "dashed")+
  
  geom_line(size=2)+

  # "#042333"  #dark blue
  # "#f7cb44" f9b641  #light yellow
  # "#f9a242" eb8055   #dark yellow
  # "#593d9c"  #ligt blue
  
  scale_color_manual( values=c("#f9b641","#eb8055","#593d9c","#042333"), 
                      labels=c("NICE  - kliniek","LCPS - Kliniek","NICE  - IC","LCPS - IC"))+
  
  scale_x_date(date_breaks = "1 week", 
               date_labels= format("%d %b"),
               limits = as.Date(c("2021-01-05", NA)))+
  
  scale_y_continuous(limits = c(0, 1.025), breaks = c(1.1,1,0.8,1.2,0.75, 0.5,0.25,0) ,labels = label_percent(1))+
  
  xlab("")+
  ylab("")+
  
  labs(title="OMT check", 
       subtitle="Nieuwe opnames: vergelijking van het lopende 7-daags gemiddelde, met de piek\n Om een stap te mogen zetten, moeten alle percentages het onder de 80% duiken.",
       caption = paste("Bron: NICE / LCPS | Plot: @YorickB | ",Sys.Date()))+
  
  guides(color = guide_legend(reverse=TRUE))+
  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  
  theme(legend.position =  "top",
        legend.background = element_rect(fill="#DAE3F3",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.margin = margin(3, 3, 3, 3),
        legend.text = element_text(colour="black", size=20, face="bold"),
        legend.direction='vertical')+
  
  theme(  plot.background = element_rect(fill = "#DAE3F3"),
          plot.title = element_text(hjust = 0.5,size = 35,face = "bold"),
          plot.subtitle = element_text(hjust = 0.5,size = 15,face = "italic"),
          panel.background = element_rect(fill = "#DAE3F3", colour = "#DAE3F3"),
          axis.text = element_text(size=14,color = "black",face = "bold"),
          axis.text.y = element_text(face="bold", color="black", size=14),
          axis.ticks = element_line(colour = "#DAE3F3", size = 1, linetype = "solid"),
          axis.ticks.length = unit(0.5, "cm"),
          axis.line = element_line(colour = "#DAE3F3"),
          panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))+
  
#ggsave("data/plots/16x_omt_check_nice.png", width=14, height = 14) 
ggsave("data/plots/16x_omt_check_nice_peak.png",width=16, height = 9)    









key <- "date"
value <- "percentage"
gathercols <- c("zkh_percentage","ic_percentage")
instroom.combi.long2 <- gather(instroom.combi, key, value, gathercols) #(2:11))

instroom.combi.long2$date = as.Date(instroom.combi.long2$date)




ggplot(instroom.combi.long2, aes(x=date, y=value, color = key))+

  
  geom_hline(yintercept=1, size = 1, linetype = "dashed")+
  geom_hline(yintercept=0.80, color = "darkgreen", size = 2)+
  
  geom_line(size=2.5, se=FALSE, span = 0.05)+
  
  scale_color_manual( values=c("#4472C4","#F4B183"), labels=c( "nieuwe opnames IC", "nieuwe opnames kliniek"))+
  
  scale_x_date(date_breaks = "1 week", 
               date_labels= format("%d %b"),
               limits = as.Date(c("2021-03-01", NA)))+
  
  scale_y_continuous(limits = c(0.7, 1.3), breaks = c(1.1,1,0.9,0.8,1.2,0.75, 0.5,0.25,0) ,labels = label_percent(1))+
  
  xlab("")+
  ylab("")+
  
  labs(title="OMT check - NICE", 
       subtitle="Vergelijking van het lopende 7-daags gemiddelde, met een week ervoor\n Om een stap te mogen zetten, moeten beide percentages het onder de 80% duiken.",
       caption = paste("Bron: NICE | Plot: @YorickB | ",Sys.Date()))+
  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  
  theme(legend.position =  "top",
        legend.background = element_rect(fill="#DAE3F3",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.margin = margin(3, 3, 3, 3),
        legend.text = element_text(colour="black", size=20, face="bold"),
        legend.direction='vertical')+
  
  theme(  plot.background = element_rect(fill = "#DAE3F3"),
          plot.title = element_text(hjust = 0.5,size = 35,face = "bold"),
          plot.subtitle = element_text(hjust = 0.5,size = 15,face = "italic"),
          panel.background = element_rect(fill = "#DAE3F3", colour = "#DAE3F3"),
          axis.text = element_text(size=14,color = "black",face = "bold"),
          axis.text.y = element_text(face="bold", color="black", size=14),
          axis.ticks = element_line(colour = "#DAE3F3", size = 1, linetype = "solid"),
          axis.ticks.length = unit(0.5, "cm"),
          axis.line = element_line(colour = "#DAE3F3"),
          panel.grid.major.y = element_line(colour= "gray", linetype = "dashed"))+
  
  
  
#  ggsave("data/plots/16x_omt_check_nice-week.png", width=14, height = 14)  
  
ggsave("data/16x_omt_check_nice-week.png",width=16, height = 9)    








