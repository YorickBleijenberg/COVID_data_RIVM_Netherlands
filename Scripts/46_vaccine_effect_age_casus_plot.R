



casus.age.dif.play <- old.casus <- read.csv("C:\\Rdir\\data-contstant\\casus_daily_age_dif.csv" ,sep=";", check.names = FALSE)

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



casus.age.dif.play$young =  casus.age.dif.play$`0-9`   + casus.age.dif.play$`10-19` +
                            casus.age.dif.play$`20-29` + casus.age.dif.play$`30-39` + casus.age.dif.play$`40-49`

casus.age.dif.play$middle = casus.age.dif.play$`50-59` + casus.age.dif.play$`60-69` 

casus.age.dif.play$old  =  casus.age.dif.play$`70-79` 
casus.age.dif.play$old80  =  casus.age.dif.play$`80-89`
casus.age.dif.play$old90p  =  casus.age.dif.play$`90+`


casus.age.dif.play$MA.young.Change   <- rollmeanr(casus.age.dif.play$young.Change  , 7, fill = 0)
casus.age.dif.play$MA.middle.Change  <- rollmeanr(casus.age.dif.play$middle.Change , 7, fill = 0)
casus.age.dif.play$MA.old.Change     <- rollmeanr(casus.age.dif.play$old.Change    , 7, fill = 0)
casus.age.dif.play$MA.old80.Change   <- rollmeanr(casus.age.dif.play$old80.Change  , 7, fill = 0)
casus.age.dif.play$MA.old90p.Change   <- rollmeanr(casus.age.dif.play$old90p.Change  , 7, fill = 0)
#vacc.effect.age.ICU$MA.old90.Change   <- rollmeanr(vacc.effect.age.ICU$old90.Change  , 7, fill = 0)


maxClinyoung <- max(casus.age.dif.play$MA.young.Change, na.rm = TRUE)
maxClinMid   <- max(casus.age.dif.play$MA.middle.Change, na.rm = TRUE)
maxClinOld   <- max(casus.age.dif.play$MA.old.Change, na.rm = TRUE)
maxClin80    <- max(casus.age.dif.play$MA.old80.Change, na.rm = TRUE)
maxClin90p    <- max(casus.age.dif.play$MA.old90p.Change, na.rm = TRUE)
#maxClin90    <- max(vacc.effect.age.ICU$MA.old90.Change, na.rm = TRUE)


casus.age.dif.play$MA.young.Change.relative  <- casus.age.dif.play$MA.young.Change /maxClinyoung
casus.age.dif.play$MA.middle.Change.relative <- casus.age.dif.play$MA.middle.Change/maxClinMid
casus.age.dif.play$MA.old.Change.relative    <- casus.age.dif.play$MA.old.Change   /maxClinOld
casus.age.dif.play$MA.old80.Change.relative  <- casus.age.dif.play$MA.old80.Change /maxClin80
casus.age.dif.play$MA.old90p.Change.relative  <- casus.age.dif.play$MA.old90p.Change /maxClin85p
#vacc.effect.age.ICU$MA.old90.Change.relative  <- vacc.effect.age.ICU$MA.old90.Change /maxClin90


relative.casus.age.dif.play <- casus.age.dif.play[ -c(2:33)]
relative.casus.age.dif.play <- relative.casus.age.dif.play[relative.casus.age.dif.play$date >"2020-10-15",]







key <- "date"
value <- "test"
relative.casus.age.dif.play.long <- gather(relative.casus.age.dif.play, key, value, 2:11)

relative.casus.age.dif.play.long$key <- as.factor(relative.casus.age.dif.play.long$key)


#colnames(casus.age.dif.play) <- c("50min","09","1090", "2029", "3039", "4049","blabla","6069","7079","8089","90p","date","unk")


#######   

ggplot(relative.casus.age.dif.play.long, aes(x=date, y= value, color = key))+
  geom_line( size=2 )
  
ggsave("data/99_test-test.png",width=16, height = 9)
