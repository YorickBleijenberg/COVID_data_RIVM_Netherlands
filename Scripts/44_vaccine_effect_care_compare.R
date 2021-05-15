new.date <- Sys.Date()
today <- Sys.Date()


### add new date ####
File_date_75 <- paste0("C:\\Rdir\\data\\", Sys.Date()-1 , "/",Sys.Date()-1, "_care_daily_diff.csv")
old.casus <- read.csv(File_date_75,sep=",")
old.casus$date <- as.Date(old.casus$date)

new.disabled.file <- paste0("https://raw.githubusercontent.com/mzelst/covid-19/master/data-rivm/nursing-homes-per-day/nursery_daily_", new.date, ".csv", sep = "")
new.casus <- fread(new.disabled.file)
new.casus$date <- as.Date(new.casus$date)
new.casus <- new.casus[,-c(2,4:6)]

cassus.age.merge.fin <- rbind(old.casus, new.casus)

File_date_72 <- paste0("data//", Sys.Date() , "/",Sys.Date(), "_care_daily_diff.csv")
write.csv(cassus.age.merge.fin, File_date_72, row.names=FALSE)


### make ready for the plot ####

casus.care.merge.fin <- cassus.age.merge.fin

colnames(casus.care.merge.fin) <- c("cases_vplg","deceased_vplg","dateInTable")

verpleeg.case.agg <- (casus.care.merge.fin %>% filter(dateInTable > "2020-07-07" ))
    
#### get new rivm data ####
# read.verpleeg.path <-paste("C:\\Rdir\\data\\",Sys.Date(),"\\", Sys.Date(), "_COVID-19_verpleeghuizen.csv", sep = "")
# copy.verpleeg <- read.csv(read.verpleeg.path,sep=";")

# copy.verpleeg$date<-as.Date(copy.verpleeg$Date_of_statistic_reported)   #Adding a date to the case

# verpleeg.sm <- copy.verpleeg[ -c(1,2,3,4,7,8)]

# colnames(verpleeg.sm) <- c("cases_vplg","deceased_vplg","dateInTable")


#### generalpop
relative <- Merged_data_short
relative <- relative[ -c(4:19)]
relative$dateInTable <- as.Date(relative$dateInTable)
#relative <- (relative %>% filter(dateInTable < today-3))

####merge
relative.table <- merge(verpleeg.case.agg,  relative, by = "dateInTable", all = TRUE)
# relative.table <- merge(verpleeg.death.agg, relative.table, by = "dateInTable", all = TRUE)

relative.table$cases_other <- relative.table$cases-relative.table$cases_vplg
relative.table$death_other <- relative.table$cases-relative.table$cases_vplg

relative.table <- (relative.table %>% filter(dateInTable > "2020-11-07" ))


relative.table$MAcases_care  <- rollmeanr(relative.table$cases_vplg,    7, fill = 0)
relative.table$MAcases_other <- rollmeanr(relative.table$cases,         7, fill = 0)
relative.table$MAdeaths_care <- rollmeanr(relative.table$deceased_vplg, 7, fill = 0)
relative.table$MAdeath_other  <- rollmeanr(relative.table$dead,          7, fill = 0)


###cases  ####
#verpleeg.case.agg <- aggregate(verpleeg.sm$cases_vplg,     by=list(dateInTable=verpleeg.sm$dateInTable), FUN=sum)
#verpleeg.case.agg <- (verpleeg.case.agg %>% filter(dateInTable > "2020-07-07" & dateInTable < today-3))   # -3
#verpleeg.case.agg$MAx <- rollmeanr(verpleeg.case.agg$x, 7, fill = 0)
#verpleeg.case.agg$ma_x_lead  <- lead(verpleeg.case.agg$MAx,3)

#verpleeg.case.agg <- verpleeg.case.agg[ -c(2,3)]
#colnames(verpleeg.case.agg) <- c("dateInTable", "cases_vplg_lead" )

#### death
#verpleeg.death.agg <- aggregate(verpleeg.sm$deceased_vplg,     by=list(dateInTable=verpleeg.sm$dateInTable), FUN=sum)
#verpleeg.death.agg <- (verpleeg.death.agg %>% filter(dateInTable > "2020-07-07" & dateInTable < today))  # -10

#verpleeg.death.agg <- casus.care.merge.fin

#verpleeg.death.agg$MAx <- rollmeanr(verpleeg.death.agg$deaths_today , 7, fill = 0)
#verpleeg.death.agg$ma_x_lead  <- lead(verpleeg.death.agg$MAx,3)

#verpleeg.death.agg <- verpleeg.death.agg[ ,-c(1,2,4)]
#colnames(verpleeg.death.agg) <- c("dateInTable", "death_vplg_lead" )





### make relative ####
maxVplgC <- max(relative.table$MAcases_care, na.rm = TRUE)
relative.table$cases_vplg_rel <- relative.table$MAcases_care/maxVplgC
maxCases <- max(relative.table$MAcases_other, na.rm = TRUE)
relative.table$cases_rel <- relative.table$MAcases_other/maxCases
maxVplgD <- max(relative.table$MAdeaths_care, na.rm = TRUE)
relative.table$death_vplg_rel <- relative.table$MAdeaths_care/maxVplgD
maxDeaths <- max(relative.table$MAdeath_other, na.rm = TRUE)
relative.table$death_rel <- relative.table$MAdeath_other/maxDeaths

relative.table <- relative.table[ -c(2:11)]


# key <- "dateInTabel"
#value <- "number"
#gathercols <- c("cases_vplg_rel","cases_rel")
# relative.table.long <- gather(relative.table, key, value, gathercols)

# relative.table.long$key <- as.factor(relative.table.long$key)



#### plot 

#ggplot(relative.table.long, aes(x=dateInTable, y=value, color = key))+
 #   geom_line(lwd = 4)+
  
  
 # geom_ribbon(data = relative.table,  aes(x=dateInTable, ymin=cases_vplg_rel, ymax=cases_rel), fill="darkred", alpha = 0.2) +
  

  #scale_y_continuous(labels = percent)+
  #scale_x_date(date_breaks = "1 months",date_labels= format("%b"),
  #limits = as.Date(c("2020-07-15", NA)))+

  #coord_cartesian(expand = FALSE)+
  
  #theme_classic()+
  
  #xlab("")+
  #ylab("")+
  
  #scale_fill_manual( values=c("#5c146e", "#fca50a", "darkgreen", "#dd513a"), labels=c("zorginstellingen", "GGD'en","Huisartsen", "ziekenhuizen" ))+
  #scale_color_brewer(palette = "RdYlBu", labels=c("zorginstellingen", "GGD'en","Huisartsen", "ziekenhuizen" ))+
  
  #geom_vline(xintercept = as.Date("2021-01-18"), linetype = "dotted") + 
  #annotate("text", x = as.Date("2021-01-19"), y = 0.8, label = "start vaccinatie verpleeghuizen", size=4, angle=0, vjust=-0.4, hjust = 0, color = "black")+
  
 # scale_color_manual( values=c("#1F968BFF", "#481567FF"), labels=c("totaal", "verpleeghuizen" ))+
  
  #labs(title = "Verpleeghuizen & Totaal",
 #      subtitle = "Besmettingen",
 #      caption = paste("Bron: RIVM | Plot: @YorickB  | ",Sys.Date()))+
  
  #theme(#legend.position = "none",   # no legend
#    legend.title = element_blank(),  ## legend title
 #   legend.position="top",
#    legend.direction = "vertical",
 #   legend.background = element_rect(fill="#f5f5f5", size=0.5, linetype="solid"))+
  
 # theme(
#plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
#    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
  #  plot.title =     element_text(hjust = 0.5 ,size = 40 ,face = "bold"),
  #  plot.subtitle =  element_text(hjust=0.5   ,size = 30 ,color = "black", face = "italic"),
 #   axis.text = element_text(size=14,color = "black",face = "bold"),
  #  axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
 #   axis.ticks.length = unit(0.5, "cm"),
  #  axis.line = element_line(colour = "#F5F5F5"),
#    panel.grid.major.x = element_blank(),
#    panel.grid.minor.x = element_blank(),
#    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))
  
#  ggsave("data/98_vaccinated_compare_cases-bla.png",width=16, height = 9)  


######



key <- "dateInTabel"
value <- "number"
gathercols <- c("death_vplg_rel", "death_rel")
relative.table.dead.long <- gather(relative.table, key, value, gathercols)

relative.table.dead.long$key <- as.factor(relative.table.dead.long$key)

#old.casus$date <- as.Date(old.casus$date)
#old.casus$MAinf <- rollmeanr(old.casus$deaths_today, 7, fill = 0)
#old.casus$MArelative <- old.casus$MAinf/60.3
#old.casus$MArelative_lead  <- lead(old.casus$MArelative,3)



### plot ###

ggplot(relative.table.dead.long, aes(x=dateInTable, y=value, color = key))+

  geom_line(lwd = 3)+
  
  #geom_line(data = old.casus, aes(x = date, y = MArelative), lwd=2, color="darkblue") +
  
#  geom_line(data = old.casus, aes(x = date, y = MArelative_lead ), lwd=2, color="black") +
 
  
   
  scale_x_date(date_breaks = "1 months",date_labels= format("%b"),
               limits = as.Date(c("2020-11-14", NA)))+

  # scale_y_continuous(limits = c(0, NA), labels = label_comma(big.mark = ".", decimal.mark = ","))+ 
  
  scale_y_continuous(labels = percent)+
  
  #coord_cartesian(expand = FALSE)+
  theme_classic()+
  
  xlab("")+
  ylab("")+
  
  #scale_color_brewer(palette = "RdYlBu", labels=c("cases", "cases verpleeg","doden", "doden verpleeg" ))+
  
 scale_color_manual( values=c("#f68f46ff", "#a65c85ff"), labels=c("doden totaal", "doden verpleeg" ))+

 # scale_color_brewer(palette = "RdYlBu", labels=c("cases", "cases verpleeg","doden", "doden verpleeg" ))+
  
    
  labs(title = "Verpleeghuizen & de rest",
       subtitle = "naar datum van rapportage",
       caption = paste("Bron: RIVM | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(#legend.position = "none",   # no legend
    legend.title = element_blank(),  ## legend title
    legend.position="top",
    legend.direction = "vertical",
    legend.background = element_rect(fill="#FDE3E3", size=0.5, linetype="solid")
  )+
  
  theme(
    plot.background = element_rect(fill = "#FDE3E3"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#FDE3E3", colour = "#FDE3E3"),
    plot.title = element_text(hjust = 0.5,size = 40,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
  #  axis.text.y = element_blank(),    # element_text(face="bold", color="black", size=14),  #, angle=45),
    axis.ticks = element_line(colour = "#FDE3E3", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#FDE3E3"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
  ggsave("data/plots/98_vaccinated_compare_death.png",width=16, height = 9)  





###3 ribbon

relative.table.title <- relative.table
relative.table.title <- (relative.table.title %>% filter( dateInTable > today-8 ))


total.cases.label <- relative.table.title$cases_rel[5]
total.cases.label <- round((total.cases.label*100), digits =1)

care.cases.label <- relative.table.title$cases_vplg_rel[1]
care.cases.label <- round((care.cases.label*100), digits =1)





subtitle.care.label <- paste0("Percentage nieuwe gevallen totaal sinds de winterpiek: ", total.cases.label, "%\n",
                              "Percentage nieuwe gevallen verpleeghuis sinds de winterpiek: ", care.cases.label , "%"
)







relative.table.short <- relative.table
relative.table.short <- (relative.table.short %>% filter( dateInTable > "2021-02-20" ))


ggplot(relative.table, aes(x=dateInTable))+
  
  geom_ribbon(data= relative.table.short, aes(x= dateInTable, ymin=cases_vplg_rel, ymax=cases_rel), fill="green", alpha = 0.5) +
  geom_line(aes(y = cases_rel), lwd=4, color="#F5F5F5") +
  geom_line(aes(y = cases_rel), lwd=3, color="#1F968BFF") +
  geom_line(aes(y = cases_vplg_rel), lwd=4, color="#F5F5F5") +
  geom_line(aes(y = cases_vplg_rel), lwd=3, color="#481567FF") +
  
  annotate("text", x = as.Date("2021-02-24"), y = 0.43, label = "Totaal",         size=5, face = "bold", color = "#1F968BFF")+
  annotate("text", x = as.Date("2021-02-12"), y = 0.18, label = "Verpleeghuizen", size=5, face = "bold", color = "#481567FF")+
  
  scale_y_continuous(labels = percent)+
  scale_x_date(date_breaks = "1 months",date_labels= format("%b"),
               limits = as.Date(c("2020-11-15", NA)))+
  
  #coord_cartesian(expand = FALSE)+
  
  theme_classic()+
  
  xlab("")+
  ylab("")+
  
  geom_vline(xintercept = as.Date("2021-01-18"), linetype = "dotted") + 
  annotate("text", x = as.Date("2021-01-19"), y = 0.8, label = "start vaccinatie verpleeghuizen", size=4, angle=0, vjust=-0.4, hjust = 0, color = "black")+
  
  #scale_color_manual( values=c("#1F968BFF", "#481567FF"), labels=c("totaal", "verpleeghuizen" ))+
  
  labs(title = "Verpleeghuizen & Totaal",
       subtitle = subtitle.care.label,
       caption = paste("Bron: RIVM | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(#legend.position = "none",   # no legend
    legend.title = element_blank(),  ## legend title
    legend.position="top",
    legend.direction = "vertical",
    legend.background = element_rect(fill="#f5f5f5", size=0.5, linetype="solid"))+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title =     element_text(hjust = 0.5 ,size = 40 ,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5   ,size = 15 ,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
ggsave("data/plots/98_leeftijd_relatief_care.png",width=16, height = 9)

