
today <- Sys.Date()

read.verpleeg.path <-paste("C:\\Rdir\\data\\",Sys.Date(),"\\", Sys.Date(), "_COVID-19_verpleeghuizen.csv", sep = "")
copy.verpleeg <- read.csv(read.verpleeg.path,sep=";")

copy.verpleeg$date<-as.Date(copy.verpleeg$Date_of_statistic_reported)   #Adding a date to the case

verpleeg.sm <- copy.verpleeg[ -c(1,2,3,4,7,8)]

colnames(verpleeg.sm) <- c("cases_vplg","deceased_vplg","dateInTable")

###cases
verpleeg.case.agg <- aggregate(verpleeg.sm$cases_vplg,     by=list(dateInTable=verpleeg.sm$dateInTable), FUN=sum)
verpleeg.case.agg <- (verpleeg.case.agg %>% filter(dateInTable > "2020-07-07" & dateInTable < today-7))
verpleeg.case.agg$MAx <- rollmeanr(verpleeg.case.agg$x, 7, fill = 0)
verpleeg.case.agg$ma_x_lead  <- lead(verpleeg.case.agg$MAx,3)

verpleeg.case.agg <- verpleeg.case.agg[ -c(2,3)]
colnames(verpleeg.case.agg) <- c("dateInTable", "cases_vplg_lead" )

#### death
verpleeg.death.agg <- aggregate(verpleeg.sm$deceased_vplg,     by=list(dateInTable=verpleeg.sm$dateInTable), FUN=sum)
verpleeg.death.agg <- (verpleeg.death.agg %>% filter(dateInTable > "2020-07-07" & dateInTable < today-14))
verpleeg.death.agg$MAx <- rollmeanr(verpleeg.death.agg$x, 7, fill = 0)
verpleeg.death.agg$ma_x_lead  <- lead(verpleeg.death.agg$MAx,3)

verpleeg.death.agg <- verpleeg.death.agg[ -c(2,3)]
colnames(verpleeg.death.agg) <- c("dateInTable", "death_vplg_lead" )

#### generalpop
relative <- Merged_data_short
relative <- relative[ -c(2:8,10,11,13,14,16:19)]
relative$dateInTable <- as.Date(relative$dateInTable)


####merge
relative.table <- merge(verpleeg.case.agg,  relative, by = "dateInTable", all = TRUE)
relative.table <- merge(verpleeg.death.agg, relative.table, by = "dateInTable", all = TRUE)

### make relative
maxVplgC <- max(relative.table$cases_vplg_lead, na.rm = TRUE)
relative.table$cases_vplg_rel <- relative.table$cases_vplg_lead/maxVplgC
maxCases <- max(relative.table$ma_c_lead, na.rm = TRUE)
relative.table$cases_rel <- relative.table$ma_c_lead/maxCases
maxVplgD <- max(relative.table$death_vplg_lead, na.rm = TRUE)
relative.table$death_vplg_rel <- relative.table$death_vplg_lead/maxVplgD
maxDeaths <- max(relative.table$ma_d_lead, na.rm = TRUE)
relative.table$death_rel <- relative.table$ma_d_lead/maxDeaths

relative.table <- relative.table[ -c(2:6)]


key <- "dateInTabel"
value <- "number"
gathercols <- c("cases_vplg_rel","cases_rel")
relative.table.long <- gather(relative.table, key, value, gathercols)

relative.table.long$key <- as.factor(relative.table.long$key)



#### plot 

ggplot(relative.table.long, aes(x=dateInTable, y=value, color = key))+
  geom_line(lwd = 3)+

  scale_y_continuous(labels = percent)+
  scale_x_date(date_breaks = "1 months",date_labels= format("%b"))+
            # limits = as.Date(c("2021-01-6", NA)))+

  #coord_cartesian(expand = FALSE)+
  theme_classic()+
    xlab("")+
  ylab("")+
  
  #scale_fill_manual( values=c("#5c146e", "#fca50a", "darkgreen", "#dd513a"), labels=c("zorginstellingen", "GGD'en","Huisartsen", "ziekenhuizen" ))+
  #scale_color_brewer(palette = "RdYlBu", labels=c("zorginstellingen", "GGD'en","Huisartsen", "ziekenhuizen" ))+
  
  scale_color_manual( values=c("#1F968BFF", "#481567FF"), labels=c("besmettingen totaal", "besmettingen verpleeghuizen" ))+
  
  labs(title = "Verpleeghuizen & de rest",
       #subtitle = "Blauw = Nieuwe besmettingen verpleeghuis \n Rood = Dagelijkse cijfers besmettingen",
       caption = paste("Bron: RIVM | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(#legend.position = "none",   # no legend
    legend.title = element_blank(),  ## legend title
    legend.position="top",
    legend.direction = "vertical",
    legend.background = element_rect(fill="#f5f5f5", size=0.5, linetype="solid"))+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title = element_text(hjust = 0.5,size = 40,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
  ggsave("data/98_vaccinated_compare_cases.png",width=16, height = 9)  



key <- "dateInTabel"
value <- "number"
gathercols <- c("death_vplg_rel", "death_rel")
relative.table.dead.long <- gather(relative.table, key, value, gathercols)

relative.table.dead.long$key <- as.factor(relative.table.dead.long$key)

### plot ###

ggplot(relative.table.dead.long, aes(x=dateInTable, y=value, color = key))+

  geom_line(lwd = 3)+
  
  scale_x_date(date_breaks = "1 months",date_labels= format("%b"))+

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
       #subtitle = "Blauw = Nieuwe doden verpleeghuis \n Rood = Dagelijkse cijfers doden",
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
  
  ggsave("data/98_vaccinated_compare_death.png",width=16, height = 9)  

