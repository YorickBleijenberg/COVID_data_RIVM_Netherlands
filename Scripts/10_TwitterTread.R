library(tidyverse)
library(rtweet)
library(data.table)

get_reply_id <- function(rel_increase) {
  my_timeline <- get_timeline(rtweet:::home_user()) ## Pull my own tweets
  reply_id <- my_timeline$status_id[1] ## Status ID for reply
  return(reply_id)
}

deE <- intToUtf8(0x00EB)
deP <- intToUtf8(0x0025)

a <- Working_Set$cases[3]   # today
b <- Working_Set$cases[2]   # yesterday
c <- Working_Set$cases[1]   # last week

e <- Working_Set$MACases[3]
f <- Working_Set$MACases[1]

h <- Working_Set$hosp[3]
i <- Working_Set$hosp[2]




j <- Working_Set$hosp[1]

k <- Working_Set$MAhosp[3]
m <- Working_Set$MAhosp[1]

n <- Working_Set$dead[3]
o <- Working_Set$dead[2]
p <- Working_Set$dead[1]

w <- Working_Set$MAdead[3]
x <- Working_Set$MAdead[1]


#### New cases Tweet ####


diff.cases.day <- abs(a-b)
diff.cases.week <- abs(a-c)

growth.cases.week <- ((e/f)-1)*100
growth.cases.week <- round(growth.cases.week, digits = 0)
doubling.cases.week <- round(log(2)/(log(e/f)/7), digits = 1)


maxValueCase <- max(copy_cases$cases, na.rm = TRUE)
dagRecordCase <- "."

if(a == maxValueCase){
  dagRecordCase <- paste(". (Nieuw dagrecord",intToUtf8(0x26a0), ")",sep = "")
}else {
  dagRecordCase <- "."}

if (a < b) {
  more.less.day.case <- paste("minder",intToUtf8(0x2B07), "dan gisteren.")
} else if (a > b) {
  more.less.day.case <- paste("meer",intToUtf8(0x2197), "dan gisteren.")
} else
  more.less.day.case <- paste("meer", intToUtf8(0x2194),"dan gisteren. (gelijk)")

if (a < c) {
  more.less.week.case <- paste("minder",intToUtf8(0x2B07), "dan een week geleden.")
} else if (a > c) {
  more.less.week.case <- paste("meer",intToUtf8(0x2197), "dan een week geleden.")
} else
  more.less.week.case <- paste("meer", intToUtf8(0x2194),"dan een week geleden. (gelijk)")


if (a < b) {
  more.less.day.case.dot <- intToUtf8(0x1F7E2)
} else if (a > b) {
  more.less.day.case.dot <- intToUtf8(0x1F534)
} else
  more.less.day.case.dot <- intToUtf8(0x1F7E1)

if (a < c) {
  more.less.week.case.dot <- intToUtf8(0x1F7E2)
} else if (a > c) {
  more.less.week.case.dot <- intToUtf8(0x1F534)
} else
  more.less.week.case.dot <- intToUtf8(0x1F7E1)



if (doubling.cases.week < 0) {
  doubling.cases.week_text <- paste("halvering")
  doubling.cases.week_dot <- intToUtf8(0x1F7E2)
} 
if (doubling.cases.week > 0) {
  doubling.cases.week_text <- paste("verdubbeling")
  doubling.cases.week_dot <- intToUtf8(0x1F534)
} 

doubling.cases.week <- abs(doubling.cases.week)

a  <- format( a, big.mark="." ,decimal.mark=",")
diff.cases.day  <- format( diff.cases.day, big.mark="." ,decimal.mark=",")
diff.cases.week  <- format( diff.cases.week, big.mark="." ,decimal.mark=",")








#### tweet.cases.tweet ####

tweet.cases.tweet <- "Nieuw gemelde besmettingen:

+%s vandaag%s

Indicatoren (exponenti%sle) groei / krimp:
%s Dat is %s %s
%s Dat is %s %s 

%s --> groeifactor: %s%s week op week.
%s --> %s: elke %s dagen."


tweet.cases.tweet <- sprintf(tweet.cases.tweet,
                             a, dagRecordCase, deE,
                             more.less.day.case.dot,  diff.cases.day,    more.less.day.case,
                             more.less.week.case.dot, diff.cases.week,   more.less.week.case,
                             doubling.cases.week_dot, growth.cases.week, deP,
                             doubling.cases.week_dot, doubling.cases.week_text, doubling.cases.week, big.mark="." ,decimal.mark="," )
Encoding(tweet.cases.tweet) <- "UTF-8"

## post_tweet(tweet.cases.tweet,  media = c("data/05_new_cases.png", "data/05_growth_cases.png", "data/07_cases_type1.png", "data/08_new_cases_WoW.png"))    # "data/06_new_cases_log.png",
post_tweet(tweet.cases.tweet,  media = c("data/05_new_cases.png", "data/05_growth_cases.png", "data/07_cases_type1.png", "data/08_new_cases_WoW.png"), in_reply_to_status_id = get_reply_id())  #







#### week.tweet ####

tweet.week.tweet <- "De gaat-deze-week-boven-vorige-week-uitkomen? grafiek."
tweet.week.tweet <- sprintf(tweet.week.tweet)
Encoding(tweet.week.tweet) <- "UTF-8"
post_tweet(tweet.week.tweet,  media = c("data/65_Cases_by_week.png"), in_reply_to_status_id = get_reply_id())  










#### kerst tweet ####

###  NL
###  250      -  6240
###  150      -  3744
###   50      -  1248
###   35      -   874

kerst.niveau.week <- df.predict.lead.kerst$MACases_2[days.to.freedom+5]   ##dag days.to.freedom
kest.niveau.text.week <- paste("waakzaam")     
if (kerst.niveau.week > 875) {                      #875
  kest.niveau.text.week <- paste("zorgelijk")
}
if (kerst.niveau.week > 2500) {
  kest.niveau.text.week <- paste("ernstig")
}
if (kerst.niveau.week > 6250) {
  kest.niveau.text.week <- paste("zeer ernstig")
}

kerst.niveau.dag <- df.predict.lead.kerst$MACases[days.to.freedom+11]
kest.niveau.text.dag <- paste("waakzaam")      
if (kerst.niveau.dag > 875) {                      #875
  kest.niveau.text.dag <- paste("zorgelijk")
}
if (kerst.niveau.dag > 2500) {
  kest.niveau.text.dag <- paste("ernstig")
}
if (kerst.niveau.dag > 6250) {
  kest.niveau.text.dag <- paste("zeer ernstig")
}


label = paste( doublingdayZ.1.text, "elke",doublingdayZ.1.int, "dagen")
label = paste( doublingdayZ.text, "elke",doublingdayZ.int, "dagen")




df.to.subset <-df.predict.lead.kerst
df.to.subset<- df.to.subset[df.to.subset$MACases<=875,]     #875
days.until.lvl2 <- df.to.subset$fixedDate[1]
today  <- Sys.Date()
days.until.lvl2<- as.vector(difftime(days.until.lvl2, today, units='days'))

days.until.lvl2[is.na(days.until.lvl2)] <- paste("> 365")

emoji_kerst <- intToUtf8(0x1F384)
emoji_snowman <- intToUtf8(0x2603)
emoji_snow <- intToUtf8(0x2744)

tweet.kerst.tweet <- "Halen we Bevrijdingsdag?

Voorspelling met 7-daags gem.:

Week-op-week (rood): 
- %s elke %s dagen,
- Waakzaam over: %s dagen
- Niveau tijdens 5 mei: [%s]


Dag-op-dag (donkerblauw):
- %s elke %s dagen,
- niveau tijdens 5 mei: [%s]
"
tweet.kerst.tweet <- sprintf(tweet.kerst.tweet,
                             
                             doublingdayZ.text,   doublingdayZ.int,
                             days.until.lvl2,
                             kest.niveau.text.dag,
                             
                             doublingdayZ.1.text, doublingdayZ.1.int,
                             kest.niveau.text.week
)
Encoding(tweet.kerst.tweet) <- "UTF-8"
post_tweet(tweet.kerst.tweet,  media = c("data/60_trendlines_cases.png"), in_reply_to_status_id = get_reply_id())  #








#### tweet.carehomes.tweet ####


tweet.carehomes.tweet <- "Verpleeghuizen
"
tweet.carehomes.tweet <- sprintf(tweet.carehomes.tweet)
Encoding(tweet.carehomes.tweet) <- "UTF-8"
post_tweet(tweet.carehomes.tweet,  media = c("data/52_Verpleeg_loc.png", "data/51_Verpleeg_dead.png", "data/50_Verpleeg_cases.png"), in_reply_to_status_id = get_reply_id())  #

















#### dead tweet ####



diff.dead.day <- abs(n-o)
diff.dead.week <- abs(n-p)
maxValuedead <- max(copy_hosp$hosp, na.rm = TRUE)
dagRecorddead <- "."
growth.dead.week <- ((w/x)-1)*100
growth.dead.week <- round(growth.dead.week, digits = 0)
doubling.dead.week <- round(log(2)/(log(w/x)/7), digits = 1)



if(n == maxValuedead){
  dagRecorddead <- paste(". (Nieuw dagrecord",intToUtf8(0x26a0), ")",sep = "")
}else {
  dagRecorddead <- "."}

if (n < o) {
  more.less.day.dead <- paste("minder",intToUtf8(0x2B07), "dan gisteren.")
} else if (n > o) {
  more.less.day.dead <- paste("meer",intToUtf8(0x2197), "dan gisteren.")
} else
  more.less.day.dead <- paste("meer", intToUtf8(0x2194),"dan gisteren. (gelijk)")

if (n < p) {
  more.less.week.dead <- paste("minder",intToUtf8(0x2B07), "dan een week geleden.")
} else if (n > p) {
  more.less.week.dead <- paste("meer",intToUtf8(0x2197), "dan een week geleden.")
} else
  more.less.week.dead <- paste("meer", intToUtf8(0x2194),"dan een week geleden. (gelijk)")


if (n < o) {
  more.less.day.dead.dot <- intToUtf8(0x1F7E2)
} else if (n > o) {
  more.less.day.dead.dot <- intToUtf8(0x1F534)
} else
  more.less.day.dead.dot <- intToUtf8(0x1F7E1)

if (n < p) {
  more.less.week.dead.dot <- intToUtf8(0x1F7E2)
} else if (n > p) {
  more.less.week.dead.dot <- intToUtf8(0x1F534)
} else
  more.less.week.dead.dot <- intToUtf8(0x1F7E1)


if (doubling.dead.week < 0) {
  doubling.dead.week_text <- paste("halvering")
  doubling.dead.week_dot <- intToUtf8(0x1F7E2)
} 
if (doubling.dead.week > 0) {
  doubling.dead.week_text <- paste("verdubbeling")
  doubling.dead.week_dot <- intToUtf8(0x1F534)
} 

doubling.dead.week <- abs(doubling.dead.week)

n  <- format( n, big.mark="." ,decimal.mark=",")
diff.dead.day  <- format( diff.dead.day, big.mark="." ,decimal.mark=",")
diff.dead.week  <- format( diff.dead.week, big.mark="." ,decimal.mark=",")

#### tweet.dead.tweet ####

tweet.dead.tweet <- "Overleden:

+%s vandaag gemeld%s

Indicatoren (exponenti%sle) groei / krimp:
%s Dat is %s %s
%s Dat is %s %s
  
%s --> groeifactor: %s%s week op week.
%s --> %s: elke %s dagen."

#Aantal overleden voor week 47:
#%s


tweet.dead.tweet <- sprintf(tweet.dead.tweet,
                            n, dagRecorddead,
                            deE,
                            more.less.day.dead.dot,  diff.dead.day,   more.less.day.dead,
                            more.less.week.dead.dot, diff.dead.week,  more.less.week.dead,
                            doubling.dead.week_dot, growth.dead.week, deP,
                            doubling.dead.week_dot, doubling.dead.week_text, doubling.dead.week)  #,
                            #diff.dead.old)
Encoding(tweet.dead.tweet) <- "UTF-8"

post_tweet(tweet.dead.tweet,  media = c("data/15_dead_diff.png",
                                        "data/02_leeftijd_heatmap-dead.png",
                                        "data/13_new_deceased.png", 
                                        "data/05_growth_dead.png"
), in_reply_to_status_id = get_reply_id()) 









#### tweet.age.tweet ####


tweet.age.tweet <- paste("Leeftijden en leeftijdsverdeling gemelde gevallen")

post_tweet(status = tweet.age.tweet, 
           media = c("data/01_leeftijd_barchart.png","data/02_leeftijd_heatmap.png", "data/03_leeftijd_relatief.png"), 
           in_reply_to_status_id = get_reply_id()
)



### Hosp tweet ####

diff.hosp.day <- abs(h-i)
diff.hosp.week <- abs(h-j)

growth.hosp.week <- Working_Set$gf_h[3]
doubling.hosp.week <- round(log(2)/(log(k/m)/7), digits = 1)
growth.hosp.week <- round(growth.hosp.week, digits = 0)

maxValueHosp <- max(copy_hosp$hosp, na.rm = TRUE)
dagRecordHosp <- "."


if(h == maxValueHosp){
  dagRecordHosp <- paste(". (Nieuw dagrecord",intToUtf8(0x26a0), ")",sep = "")
}else {
  dagRecordHosp <- "."}

if (h < i) {
  more.less.day.hosp <- paste("minder",intToUtf8(0x2B07), "dan gisteren.")
} else if (h > i) {
  more.less.day.hosp <- paste("meer",intToUtf8(0x2197), "dan gisteren.")
} else
  more.less.day.hosp <- paste("meer", intToUtf8(0x2194),"dan gisteren. (gelijk)")

if (h < j) {
  more.less.week.hosp <- paste("minder",intToUtf8(0x2B07), "dan een week geleden.")
} else if (h > j) {
  more.less.week.hosp <- paste("meer",intToUtf8(0x2197), "dan een week geleden.")
} else
  more.less.week.hosp <- paste("meer", intToUtf8(0x2194),"dan een week geleden. (gelijk)")


if (h < i) {
  more.less.day.hosp.dot <- intToUtf8(0x1F7E2)
} else if (h > i) {
  more.less.day.hosp.dot <- intToUtf8(0x1F534)
} else
  more.less.day.hosp.dot <- intToUtf8(0x1F7E1)

if (h < j) {
  more.less.week.hosp.dot <- intToUtf8(0x1F7E2)
} else if (h > j) {
  more.less.week.hosp.dot <- intToUtf8(0x1F534)
} else
  more.less.week.hosp.dot <- intToUtf8(0x1F7E1)

if (doubling.hosp.week < 0) {
  doubling.hosp.week_text <- paste("halvering")
  doubling.hosp.week_dot <- intToUtf8(0x1F7E2)
} 
if (doubling.hosp.week > 0) {
  doubling.hosp.week_text <- paste("verdubbeling")
  doubling.hosp.week_dot <- intToUtf8(0x1F534)
} 

doubling.hosp.week <- abs(doubling.hosp.week)


h  <- format( h, big.mark="." ,decimal.mark=",")
diff.hosp.day  <- format( diff.hosp.day, big.mark="." ,decimal.mark=",")
diff.hosp.week  <- format( diff.hosp.week, big.mark="." ,decimal.mark=",")

#### tweet.hosp.tweet ####

tweet.hosp.tweet <- "Nieuw gemelde opnames ziekenhuis (RIVM):

+%s vandaag%s

Indicatoren (exponenti%sle) groei / krimp:
%s Dat is %s %s
%s Dat is %s %s

%s --> groeifactor: %s%s week op week.
%s --> %s: elke %s dagen."


tweet.hosp.tweet <- sprintf(tweet.hosp.tweet,
                            h, dagRecordHosp,
                            deE,
                            more.less.day.hosp.dot,  diff.hosp.day,   more.less.day.hosp,
                            more.less.week.hosp.dot, diff.hosp.week,  more.less.week.hosp,
                            doubling.hosp.week_dot, growth.hosp.week,deP,
                            doubling.hosp.week_dot, doubling.hosp.week_text, doubling.hosp.week )
Encoding(tweet.hosp.tweet) <- "UTF-8"
post_tweet(tweet.hosp.tweet,  media = c("data/02_leeftijd_heatmap-hosp.png","data/09_new_hosp.png", "data/05_growth_hosp.png"), in_reply_to_status_id = get_reply_id()) 




####  tweet.growth.tweet #####  


gf_c_last <-Working_Set$gf_c[3]
gf_h_last <-Working_Set$gf_h[3]
gf_d_last <-Working_Set$gf_d[3]


growth.cases.day <- round((((Working_Set$MACases[3]/Working_Set$MACases[2])-1)*100), digits = 1)
growth.cases.day_act <- round((((Working_Set$cases[3]/Working_Set$cases[2])-1)*100), digits = 1)


doubling.dead.day_dot <- intToUtf8(0x1F7E1)
doubling.dead.day_act_dot <- intToUtf8(0x1F7E1)
if (growth.cases.day < 0) {
  doubling.dead.day_dot <- intToUtf8(0x1F7E2)
} 
if (growth.cases.day > 0) {
  doubling.dead.day_dot <- intToUtf8(0x1F534)
} 

if (growth.cases.day_act < 0) {
  doubling.dead.day_act_dot <- intToUtf8(0x1F7E2)
} 
if (growth.cases.day_act > 0) {
  doubling.dead.day_act_dot <- intToUtf8(0x1F534)
} 


tweet.growth.tweet <- "Groeifactor week op week:

%s besmettingen:   %s %s 
%s opnames (RIVM): %s %s
%s overleden:      %s %s

Groeifactor dag-op-dag (7-daags gemiddelde):
%s Besmettingen:   %s %s

Groeifactor dag-op-dag
%s Besmettingen:   %s %s
"


tweet.growth.tweet <- sprintf(tweet.growth.tweet,
                              doubling.cases.week_dot, gf_c_last,deP,
                              doubling.hosp.week_dot, gf_h_last,deP,
                              doubling.dead.week_dot, gf_d_last,deP,
                              doubling.dead.day_dot, growth.cases.day,deP,
                              doubling.dead.day_act_dot, growth.cases.day_act, deP)
Encoding(tweet.growth.tweet) <- "UTF-8"
# post_tweet(tweet.growth.tweet,  media = c("data/07_new_cases_DoD.png", "data/05_growth_cases.png", "data/05_growth_hosp.png","data/05_growth_dead.png"), in_reply_to_status_id = get_reply_id()) 



#### tweet.datums.tweet ####

PersCoKroeg = as.Date("2020-09-18",'%Y-%m-%d')
PersCoKroegDays <- as.numeric(difftime(Sys.Date(),PersCoKroeg, units = c("days")))
PersCoPaniek = as.Date("2020-09-28",'%Y-%m-%d')
PersCoPaniekDays <- as.numeric(difftime(Sys.Date(),PersCoPaniek, units = c("days")))
PersCoSemiLockdown = as.Date("2020-10-13",'%Y-%m-%d')
PersCoSemiLockdownDays <- as.numeric(difftime(Sys.Date(),PersCoSemiLockdown, units = c("days")))
PersCoSemitwoWeeks = as.Date("2020-11-03",'%Y-%m-%d')
PersCoSemitwoWeeksDays <- as.numeric(difftime(Sys.Date(),PersCoSemitwoWeeks, units = c("days")))
PersCoSemitwoWeeksdone = as.Date("2020-11-17",'%Y-%m-%d')
PersCoSemitwoWeeksdoneDays <- as.numeric(difftime(Sys.Date(),PersCoSemitwoWeeksdone, units = c("days")))  
PersCoDoNothing = as.Date("2020-12-08",'%Y-%m-%d')
PersCoDoNothingDays <- as.numeric(difftime(Sys.Date(),PersCoDoNothing, units = c("days")))  
PersColockdown = as.Date("2020-12-14",'%Y-%m-%d')
PersColockdownDays <- as.numeric(difftime(Sys.Date(),PersColockdown, units = c("days")))
PersCoCurfew = as.Date("2021-01-20",'%Y-%m-%d')
PersCoCurfewDays <- as.numeric(difftime(Sys.Date(),PersCoCurfew, units = c("days")))  

tweet.data.tweet <- "Dagen sinds persco:

%s -Kroeg uurtje eerder dicht - regionale maatregelen

%s -We gaan voor R=0,9 - landelijke maatregelen

%s -Semi-lockdown

%s -Verzwaring semi-lockdown

%s -Einde verzwaring semi-lockdown

%s -Zorgelijk, maar we doen niets.

%s -Lockdown

%s -Avondklok"


tweet.data.tweet <- sprintf(tweet.data.tweet,
                            PersCoKroegDays,
                            PersCoPaniekDays,
                            PersCoSemiLockdownDays,
                            PersCoSemitwoWeeksDays,
                            PersCoSemitwoWeeksdoneDays,
                            PersCoDoNothingDays,
                            PersColockdownDays,
                            PersCoCurfewDays
)
Encoding(tweet.data.tweet) <- "UTF-8"
post_tweet(tweet.data.tweet, in_reply_to_status_id = get_reply_id()) 


#### tweet.cases.diff.tweet ####

tweet.cases.diff.tweet <- "1) provincies 
2) Besmette personen toegevoegd / verschil met gisteren.
3) Besmette personen, verschil met gisteren.  | maandagen
4) CoronaMelder App authorisaties
"
tweet.cases.diff.tweet <- sprintf(tweet.cases.diff.tweet)
Encoding(tweet.cases.diff.tweet) <- "UTF-8"
post_tweet(tweet.cases.diff.tweet,  media = c("data/20_prov_new-test.png", "data/07_cases_diff.png", "data/07_cases_type1-monday.png", "data/81_coronamelder.png"), in_reply_to_status_id = get_reply_id())  #

###    media = c("data/17_IC_only.png", "data/16_IC_hosp.png")


#### vakantie tweet ####

tweet.vakantie.tweet <- "Speciaal voor de mensen die graag regio Noord, met regio's Midden en Zuid willen vergelijken:  voor u heb ik hier de vergelijking!

Kun je hier conclusies uit trekken?

Nee. 

(Maar het is wel leuk om te zien)"



tweet.vakantie.tweet <- sprintf(tweet.vakantie.tweet)
Encoding(tweet.vakantie.tweet) <- "UTF-8"
##  post_tweet(tweet.vakantie.tweet,  media = c("data/40_niet-noord-raw.png"), in_reply_to_status_id = get_reply_id())  #



#### all muni tweet ####

tweet.all.muni.tweet <- "Alle gemeenten

Groen: dalende trend
Geel: Meh
Donkerrood: stijgende trend
Rood:  Alarm!

"
tweet.all.muni.tweet <- sprintf(tweet.all.muni.tweet)
Encoding(tweet.all.muni.tweet) <- "UTF-8"
post_tweet(tweet.all.muni.tweet,  media = c("data/75_Municipality-day-phd.png"), in_reply_to_status_id = get_reply_id())  #


#"
#7-daags gemiddelde (7ma)

#Groen: 
#Het 7ma is lager dan 7 EN 14 dagen geleden

#Donkerrood:
#het 7ma is hoger dan 7 OF 14 dagen geleden

#Rood:
#Het 7ma is (veel) hoger dan 7 dagen geleden

#Geel:
#De rest

#  kleur <- paste(yellow)
#  if (((aa > cc+2) | (aa > dd+3))&(aa < dd+13)) {
#    kleur <- paste(red)
#  } else if (aa > dd+13) {
#    kleur <- paste(help)
#  } else if (((aa < cc-2) & (aa < dd-2))| (aa<1)) {
#    kleur <- paste(green)
#  }
  
#"




#### Combi.tweet ####

tweet.combi.2.tweet <- "1) 16 grote steden
2) provincies
3) Routekaart
4) Weektotalen"
tweet.combi.2.tweet <- sprintf(tweet.combi.2.tweet)
Encoding(tweet.combi.2.tweet) <- "UTF-8"
post_tweet(tweet.combi.2.tweet,  media = c("data/18_city_new.png", "data/20_prov_new-no-color.png","data/60_routekaart.png", "data/65_Cases_by_week_facet-grid.png" ), in_reply_to_status_id = get_reply_id())  #

#### 16 cities tweet ####

tweet.16city.tweet <- "Nieuwe gevallen in de 16 grote steden"
tweet.16city.tweet <- sprintf(tweet.16city.tweet)
Encoding(tweet.16city.tweet) <- "UTF-8"
#post_tweet(tweet.16city.tweet,  media = c("data/18_city_new.png"), in_reply_to_status_id = get_reply_id())  #

#### province tweet ####

tweet.16city.tweet <- "Nieuwe gevallen in de provincies"
tweet.16city.tweet <- sprintf(tweet.16city.tweet)
Encoding(tweet.16city.tweet) <- "UTF-8"
#post_tweet(tweet.16city.tweet,  media = c("data/20_prov_phd.png"), in_reply_to_status_id = get_reply_id())  #


#### Week totals tweet ####

tweet.week_num.tweet <- "Nieuwe gevallen per week"
tweet.week_num.tweet <- sprintf(tweet.week_num.tweet)
Encoding(tweet.week_num.tweet) <- "UTF-8"
#post_tweet(tweet.week_num.tweet,  media = c("data/65_Cases_by_week_test_ Darjeeling1"), in_reply_to_status_id = get_reply_id())  #




#### vaccine tweet ####

tweet.vaccince.tweet <- "De weg naar groepsimmuniteit

Noot: Op dit moment hebben we alleen BioNTech/Pfizer vaccins"
tweet.vaccince.tweet <- sprintf(tweet.vaccince.tweet)
Encoding(tweet.vaccince.tweet) <- "UTF-8"
#post_tweet(tweet.vaccince.tweet,  media = c("data/90_vaccine_deliverd.png"), in_reply_to_status_id = get_reply_id())


#### Week Christmas deaths tweet ####

tweet.christ.death.tweet <- "Kerstdoden

https://twitter.com/YorickB/status/1341799378458202117"
tweet.christ.death.tweet <- sprintf(tweet.christ.death.tweet)
Encoding(tweet.christ.death.tweet) <- "UTF-8"
#post_tweet(tweet.christ.death.tweet,  media = c("data/88_christ_death.png"), in_reply_to_status_id = get_reply_id())



#### Lansingerland tweet ####

tweet.Lansingerland.tweet <- "Lansingerland"
tweet.Lansingerland.tweet <- sprintf(tweet.Lansingerland.tweet)
Encoding(tweet.Lansingerland.tweet) <- "UTF-8"
post_tweet(tweet.Lansingerland.tweet,  media = c("data/18_city_new_Lansingerland.png"), in_reply_to_status_id = get_reply_id())





