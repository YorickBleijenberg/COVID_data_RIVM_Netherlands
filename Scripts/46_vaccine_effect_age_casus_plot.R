
new.date <- Sys.Date()
old.date <- new.date-1


File_date_46 <- paste0("data/",old.date,"/",old.date,"_casus_daily_age_dif.csv")
cassus.age.spread.base  <- read.csv(File_date_46 ,sep=";", check.names = FALSE)


old.casus.file <-paste("C:\\Rdir\\rivm-data\\COVID-19_casus_landelijk_", old.date, ".csv", sep = "")
old.casus <- read.csv(old.casus.file,sep=";")
old.casus$Date_statistics <- as.Date(old.casus$Date_statistics)

new.casus.file <-paste("C:\\Rdir\\rivm-data\\COVID-19_casus_landelijk_", new.date, ".csv", sep = "")
new.casus <- read.csv(new.casus.file,sep=";")
new.casus$Date_statistics <- as.Date(new.casus$Date_statistics)

old.casus.age <- count(old.casus,Agegroup)
new.casus.age <- count(new.casus,Agegroup)

colnames(new.casus.age) <- c("Agegroup","n2")

cassus.age.merge <- merge(old.casus.age,new.casus.age)

cassus.age.merge$diff <- cassus.age.merge$n2-cassus.age.merge$n

#sum.cases =  sum(cassus.age.merge$diff)
cassus.age.merge.short <- cassus.age.merge[ -c(2,3)]

date.text = as.character(new.date)
cassus.age.merge.fin <- rbind(c("date",date.text), cassus.age.merge.short)

cassus.age.spread <- spread(cassus.age.merge.fin, Agegroup, diff)

cassus.age.spread.base <- rbind(cassus.age.spread.base, cassus.age.spread)



File_date_99 <- paste0("data/",new.date,"/",new.date,"_casus_daily_age_dif.csv")
write.csv2(cassus.age.spread.base, File_date_99, row.names=FALSE)
File_date_99 <- paste0("data//casus_vac_effect/casus_daily_age_dif.csv")
write.csv(cassus.age.spread.base, File_date_99, row.names=FALSE)





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


### make agegroups
casus.age.dif.play$young =  casus.age.dif.play$`0-9`   + casus.age.dif.play$`10-19` +
                            casus.age.dif.play$`20-29` + casus.age.dif.play$`30-39` + casus.age.dif.play$`40-49`
casus.age.dif.play$middle = casus.age.dif.play$`50-59`
casus.age.dif.play$sixty = casus.age.dif.play$`60-69` 
casus.age.dif.play$old  =  casus.age.dif.play$`70-79` 
casus.age.dif.play$old80  =  casus.age.dif.play$`80-89`
casus.age.dif.play$old90p  =  casus.age.dif.play$`90+`


casus.age.dif.play$MA.young.Change   <- rollmeanr(casus.age.dif.play$young   , 7, fill = 0)
casus.age.dif.play$MA.middle.Change  <- rollmeanr(casus.age.dif.play$middle  , 7, fill = 0)
casus.age.dif.play$MA.sixty.Change  <- rollmeanr(casus.age.dif.play$sixty  , 7, fill = 0)
casus.age.dif.play$MA.old.Change     <- rollmeanr(casus.age.dif.play$old     , 7, fill = 0)
casus.age.dif.play$MA.old80.Change   <- rollmeanr(casus.age.dif.play$old80   , 7, fill = 0)
casus.age.dif.play$MA.old90p.Change   <- rollmeanr(casus.age.dif.play$old90p , 7, fill = 0)
#vacc.effect.age.ICU$MA.old90.Change   <- rollmeanr(vacc.effect.age.ICU$old90.Change  , 7, fill = 0)


maxClinyoung <- 7172  # max(casus.age.dif.play$MA.young.Change, na.rm = TRUE)
maxClinMid   <- 2032  # max(casus.age.dif.play$MA.middle.Change, na.rm = TRUE)
maxClinSixty <- 1217  # max(casus.age.dif.play$MA.sixty.Change, na.rm = TRUE)
maxClinOld   <- 733   # max(casus.age.dif.play$MA.old.Change, na.rm = TRUE)
maxClin80    <- 470   # max(casus.age.dif.play$MA.old80.Change, na.rm = TRUE)
maxClin90p   <- 165   # max(casus.age.dif.play$MA.old90p.Change, na.rm = TRUE)
#maxClin90    <- max(vacc.effect.age.ICU$MA.old90.Change, na.rm = TRUE)


casus.age.dif.play$MA.young.Change.relative  <- casus.age.dif.play$MA.young.Change /maxClinyoung
casus.age.dif.play$MA.middle.Change.relative <- casus.age.dif.play$MA.middle.Change/maxClinMid
casus.age.dif.play$MA.sixty.Change.relative <- casus.age.dif.play$MA.sixty.Change/maxClinSixty
casus.age.dif.play$MA.old.Change.relative    <- casus.age.dif.play$MA.old.Change   /maxClinOld
casus.age.dif.play$MA.old80.Change.relative  <- casus.age.dif.play$MA.old80.Change /maxClin80
casus.age.dif.play$MA.old90p.Change.relative  <- casus.age.dif.play$MA.old90p.Change /maxClin90p
#vacc.effect.age.ICU$MA.old90.Change.relative  <- vacc.effect.age.ICU$MA.old90.Change /maxClin90


#relative.casus.age.dif.play <- casus.age.dif.play[ -c(1:11,13:23)]
casus.age.dif.play <- casus.age.dif.play[casus.age.dif.play$date >"2020-10-08",]
casus.age.dif.play.short <- casus.age.dif.play[casus.age.dif.play$date >"2021-02-16",]




#key <- "date"
#value <- "test"
#relative.casus.age.dif.play.long <- gather(relative.casus.age.dif.play, key, value, 2:6)
#relative.casus.age.dif.play.long$key <- as.factor(relative.casus.age.dif.play.long$key)

#colnames(casus.age.dif.play) <- c("50min","09","1090", "2029", "3039", "4049","blabla","6069","7079","8089","90p","date","unk")

dag.label <- as.Date(Sys.Date()+5)

nine.Plus.label <- last(casus.age.dif.play$MA.old90p.Change.relative)
five.min.label <- last(casus.age.dif.play$MA.young.Change.relative)
nine.Plus.label <- round((nine.Plus.label*100), digits =1)
five.min.label <- round((five.min.label*100), digits =1)

subtitle.label <- paste0("Verschil tov gisteren per leeftijdsgroep, ahv casusdata; 7-daags gemiddelde\n\n", 
                         "Percentage nieuwe gevallen 50- sinds de winterpiek: ", five.min.label, "%\n",
                         "Percentage nieuwe gevallen 90+ sinds de winterpiek: ", nine.Plus.label, "%"
                         )


#######

label.five.min.label  <- last(casus.age.dif.play$MA.young.Change.relative  )
label.five.label      <- last(casus.age.dif.play$MA.middle.Change.relative  )
label.six.label       <- last(casus.age.dif.play$MA.sixty.Change.relative  )
label.zeven.label     <- last(casus.age.dif.play$MA.old.Change.relative  )
label.eight.label     <- last(casus.age.dif.play$MA.old80.Change.relative  )
label.nine.label      <- last(casus.age.dif.play$MA.old90p.Change.relative  )

#######

ggplot(casus.age.dif.play)+
 
  geom_ribbon(data  = casus.age.dif.play.short, 
              aes(x = date  , ymin=MA.old90p.Change.relative, ymax=MA.young.Change.relative), 
               fill = "blue" , alpha = 0.25) +
  
  geom_line(  aes(x= date, MA.young.Change.relative),  lwd=4, color="#F5F5F5")+
  geom_line(  aes(x= date, MA.young.Change.relative),  lwd=3, color="#B8DE29FF")+
  geom_line(  aes(x= date, MA.middle.Change.relative),  lwd=4, color="#F5F5F5")+
  geom_line(  aes(x= date, MA.middle.Change.relative),  lwd=3, color="#55C667FF")+
  
  geom_line(  aes(x= date, MA.sixty.Change.relative),  lwd=4, color="#F5F5F5")+
  geom_line(  aes(x= date, MA.sixty.Change.relative),  lwd=3, color="#29AF7FFF")+
  
  geom_line(  aes(x= date, MA.old.Change.relative),    lwd=4, color="#F5F5F5")+
  geom_line(  aes(x= date, MA.old.Change.relative),    lwd=3, color="#1F968BFF")+
  geom_line(  aes(x= date, MA.old80.Change.relative),    lwd=4, color="#F5F5F5")+
  geom_line(  aes(x= date, MA.old80.Change.relative),    lwd=3, color="#404788FF")+
  geom_line(  aes(x= date, MA.old90p.Change.relative),    lwd=4, color="#F5F5F5")+
  geom_line(  aes(x= date, MA.old90p.Change.relative),    lwd=3, color="#440154FF")+
  
  annotate("text", x = as.Date(dag.label), y = label.five.min.label, label = "- 50 min",  size=5, face = "bold", color = "#B8DE29FF")+
 # annotate("text", x = as.Date(dag.label), y = label.five.nine.label, label = " 50-69" ,  size=5.2, face = "bold", color = "black")+
  annotate("text", x = as.Date(dag.label), y = label.five.label, label = "- 50-59" ,  size=5, face = "bold", color = "#55C667FF")+
  
  annotate("text", x = as.Date(dag.label), y = label.six.label, label = "- 60-69" ,  size=5, face = "bold", color = "#29AF7FFF")+
  

  annotate("text", x = as.Date(dag.label), y = label.zeven.label, label = "- 70-79" ,  size=5, face = "italic", color = "#1F968BFF")+
  annotate("text", x = as.Date(dag.label), y = label.eight.label, label = "- 80-89" ,  size=5, face = "bold", color = "#404788FF")+
  annotate("text", x = as.Date(dag.label), y = label.nine.label, label = "- 90+"   ,  size=5, face = "bold", color = "#440154FF")+
  
  scale_y_continuous(limits = c(0, NA),labels = percent)+
  scale_x_date(date_breaks  = "1 months",date_labels= format("%b"),
                    limits  = as.Date(c("2020-10-01", NA)))+
  
  theme_classic()+
  
  xlab("")+
  ylab("")+

#  geom_vline(xintercept = as.Date("2021-02-21"), linetype = "dotted") + 
 # annotate("text", x  = as.Date("2021-02-22"), y = 0.7, label = "57% \nvan de 85+ers heeft \neen eerste prik ontvangen \nen 61% van de 90+ers \n(thuiswonend)", size=3, angle=0, vjust=-0.4, hjust = 0, color = "black")+
  
  labs(title     = "Besmettingen",
       subtitle = subtitle.label,
       caption   = paste("Bron: RIVM | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(#legend.position = "none",   # no legend
    legend.title       = element_blank(),  ## legend title
    legend.position    = "top",
    legend.direction   = "vertical",
    legend.background  = element_rect(fill="#f5f5f5", size=0.5, linetype="solid"))+
  
  theme(
    plot.background    = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background   = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title         = element_text(hjust = 0.5 ,size = 40 ,face = "bold"),
    plot.subtitle      = element_text(hjust=0.5   ,size = 15 ,color = "black", face = "italic"),
    axis.text          = element_text(size=14,color = "black",face = "bold"),
    axis.ticks         = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length  = unit(0.5, "cm"),
    axis.line          = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
ggsave("data/plots/99_leeftijd_relatief_case.png",width=16, height = 9)

