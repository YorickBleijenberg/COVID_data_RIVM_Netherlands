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
                             doubling.cases.week_dot, doubling.cases.week_text, doubling.cases.week )
Encoding(tweet.cases.tweet) <- "UTF-8"

## post_tweet(tweet.cases.tweet,  media = c("data/05_new_cases.png", "data/05_growth_cases.png", "data/07_cases_type1.png", "data/08_new_cases_WoW.png"))    # "data/06_new_cases_log.png",
post_tweet(tweet.cases.tweet,  media = c("data/05_new_cases.png", "data/05_growth_cases.png", "data/07_cases_type1.png", "data/08_new_cases_WoW.png"), in_reply_to_status_id = get_reply_id())  #



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

#### tweet.dead.tweet ####

tweet.dead.tweet <- "Overleden:

+%s vandaag gemeld%s

Indicatoren (exponenti%sle) groei / krimp:
%s Dat is %s %s
%s Dat is %s %s
  
%s --> groeifactor: %s%s week op week.
%s --> %s: elke %s dagen."


tweet.dead.tweet <- sprintf(tweet.dead.tweet,
                            n, dagRecorddead,
                            deE,
                            more.less.day.dead.dot,  diff.dead.day,   more.less.day.dead,
                            more.less.week.dead.dot, diff.dead.week,  more.less.week.dead,
                            doubling.dead.week_dot, growth.dead.week, deP,
                            doubling.dead.week_dot, doubling.dead.week_text, doubling.dead.week)
Encoding(tweet.dead.tweet) <- "UTF-8"

post_tweet(tweet.dead.tweet,  media = c("data/15_dead_diff.png",
                                        "data/02_leeftijd_heatmap-dead.png",
                                        "data/13_new_deceased.png", 
                                        "data/05_growth_dead.png"
), in_reply_to_status_id = get_reply_id()) 

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



#### tweet.age.tweet ####


tweet.age.tweet <- paste("Leeftijden en leeftijdsverdeling gemelde gevallen")

post_tweet(status = tweet.age.tweet, 
           media = c("data/01_leeftijd_barchart.png","data/02_leeftijd_heatmap.png", "data/03_leeftijd_relatief.png"), 
           in_reply_to_status_id = get_reply_id()
)



####  tweet.growth.tweet #####  


gf_c_last <-Working_Set$gf_c[3]
gf_h_last <-Working_Set$gf_h[3]
gf_d_last <-Working_Set$gf_d[3]


growth.cases.day <- round((((Working_Set$MACases[3]/Working_Set$MACases[2])-1)*100), digits = 1)
growth.cases.day_act <- round((((Working_Set$cases[3]/Working_Set$cases[2])-1)*100), digits = 1)



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
post_tweet(tweet.growth.tweet,  media = c("data/07_new_cases_DoD.png", "data/05_growth_cases.png", "data/05_growth_hosp.png","data/05_growth_dead.png"), in_reply_to_status_id = get_reply_id()) 





#### tweet.datums.tweet ####

PersCoKroeg = as.Date("2020-09-18",'%Y-%m-%d')
PersCoKroegDays <- as.numeric(difftime(Sys.Date(),PersCoKroeg, units = c("days")))
PersCoPaniek = as.Date("2020-09-28",'%Y-%m-%d')
PersCoPaniekDays <- as.numeric(difftime(Sys.Date(),PersCoPaniek, units = c("days")))
PersCoSemiLockdown = as.Date("2020-10-13",'%Y-%m-%d')
PersCoSemiLockdownDays <- as.numeric(difftime(Sys.Date(),PersCoSemiLockdown, units = c("days")))  

tweet.data.tweet <- "Dagen sinds:

[%s] de persCo: 'kroeg uurtje eerder dicht' - regionale maatregelen

[%s] de persoCo: 'We gaan voor R=0,9' - landelijke maatregelen

[%s] de persoCo: 'Semi-lockdown'"


tweet.data.tweet <- sprintf(tweet.data.tweet,
                            PersCoKroegDays,
                            PersCoPaniekDays,
                            PersCoSemiLockdownDays
)
Encoding(tweet.data.tweet) <- "UTF-8"
post_tweet(tweet.data.tweet, in_reply_to_status_id = get_reply_id()) 


#### tweet.cases.diff.tweet ####

tweet.cases.diff.tweet <- "1) provincies 
2) Besmette personen toegevoegd / verschil met gisteren.
3) Besmette personen, verschil met gisteren.  | maandagen
"
tweet.cases.diff.tweet <- sprintf(tweet.cases.diff.tweet)
Encoding(tweet.cases.diff.tweet) <- "UTF-8"
post_tweet(tweet.cases.diff.tweet,  media = c("data/18_Province_cumulative_log.png", "data/07_cases_diff.png", "data/07_cases_type1-monday.png"), in_reply_to_status_id = get_reply_id())  #

###    media = c("data/17_IC_only.png", "data/16_IC_hosp.png")


#### vakantie tweet ####

tweet.vakantie.tweet <- "Speciaal voor de mensen die graag regio Noord, met regio's Midden en Zuid willen vergelijken:  voor u heb ik hier de vergelijking!

Kun je hier conclusies uit trekken?

Nee. 

(Maar het is wel leuk om te zien)"



tweet.vakantie.tweet <- sprintf(tweet.vakantie.tweet)
Encoding(tweet.vakantie.tweet) <- "UTF-8"
post_tweet(tweet.vakantie.tweet,  media = c("data/40_niet-noord-phd.png"), in_reply_to_status_id = get_reply_id())  #

#### 16 cities tweet ####

tweet.16city.tweet <- "Nieuwe gevallen in de 16 grote steden"
tweet.16city.tweet <- sprintf(tweet.16city.tweet)
Encoding(tweet.16city.tweet) <- "UTF-8"
post_tweet(tweet.16city.tweet,  media = c("data/18_city_new.png"), in_reply_to_status_id = get_reply_id())  #

#### province tweet ####


tweet.16city.tweet <- "Nieuwe gevallen in de provincies"
tweet.16city.tweet <- sprintf(tweet.16city.tweet)
Encoding(tweet.16city.tweet) <- "UTF-8"
post_tweet(tweet.16city.tweet,  media = c("data/20_prov_new.png"), in_reply_to_status_id = get_reply_id())  #


