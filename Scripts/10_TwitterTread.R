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
  dagRecordCase <- paste(". (Nieuw dagrecord",intToUtf8(0x1F973), ")",sep = "")
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

my_timeline <- get_my_timeline()  
reply_id <- my_timeline$status_id[1]
## post_tweet(tweet.cases.tweet,  media = c("data/05_new_cases.png", "data/05_growth_cases.png", "data/07_cases_type1.png", "data/08_new_cases_WoW.png"))    # "data/06_new_cases_log.png",
post_tweet(tweet.cases.tweet,  media = c("data/plots/05_new_cases.png",
                                         "data/05_growth_cases.png",
                                         "data/07_cases_type1.png",
                                         "data/08_new_cases_WoW.png"), in_reply_to_status_id = reply_id)  #











#### week.tweet ####

tweet.week.tweet <-  "Nieuwe gevallen" # "De gaan-we-week-4-inhalen? grafiek."
tweet.week.tweet <- sprintf(tweet.week.tweet)
Encoding(tweet.week.tweet) <- "UTF-8"

my_timeline <- get_my_timeline()  
reply_id <- my_timeline$status_id[1]
post_tweet(tweet.week.tweet,  media = c("data/plots/65_Cases_by_week.png",
                                        "data/plots/60_trendlines_cases.png",
                                        "data/02_leeftijd_heatmap.png",
                                        "data/03_leeftijd_relatief.png"
                                        ), in_reply_to_status_id = reply_id)  





#### tweet.summer.zoom.tweet ####

tweet.summer.zoom.tweet <-  "Zomer/najaar 2021




" 
tweet.summer.zoom.tweet <- sprintf(tweet.summer.zoom.tweet
                                )
Encoding(tweet.summer.zoom.tweet) <- "UTF-8"
my_timeline <- get_my_timeline()  
reply_id <- my_timeline$status_id[1]
post_tweet(tweet.summer.zoom.tweet,  media = c("data/plots/05_new_cases_2021.png",
                                               "data/07_cases_type1_summer_2021.png",
                                               "data/plots/22_tests_ggd_daily_zoom.png"
), in_reply_to_status_id = reply_id)  









#### tweet.carehomes.tweet ####


 tweet.carehomes.tweet <- "Verpleeghuizen

- Locaties met een besmetting
- nieuwe gevallen (laatste 2 dagen worden nog aangevuld)
- Aantal doden (laatste 2 weken worden nog aangevuld)"
tweet.carehomes.tweet <- sprintf(tweet.carehomes.tweet)
Encoding(tweet.carehomes.tweet) <- "UTF-8"
my_timeline <- get_my_timeline()  
reply_id <- my_timeline$status_id[1]
post_tweet(tweet.carehomes.tweet,  media = c("data/52_Verpleeg_loc.png",
                                             "data/51_Verpleeg_dead.png",
                                             "data/50_Verpleeg_cases.png"), in_reply_to_status_id = reply_id)  #





#### dead tweet ####

diff.dead.day <- abs(n-o)
diff.dead.week <- abs(n-p)
maxValuedead <- max(copy_dead$dead, na.rm = TRUE)
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
my_timeline <- get_my_timeline()  
reply_id <- my_timeline$status_id[1]

post_tweet(tweet.dead.tweet,  media = c("data/plots/15_dead_diff.png",
                                        "data/02_leeftijd_heatmap-dead.png",
                                        "data/13_new_deceased.png", 
                                        "data/05_growth_dead.png"
), in_reply_to_status_id = reply_id) 

















#### all muni tweet ####

tweet.all.muni.tweet <- "Alle gemeenten

Groen: dalende trend
Geel: Meh
Donkerrood: stijgende trend
Rood:  Alarm!

"
tweet.all.muni.tweet <- sprintf(tweet.all.muni.tweet)
Encoding(tweet.all.muni.tweet) <- "UTF-8"
#post_tweet(tweet.all.muni.tweet,  media = c("data/plots/75_Municipality-day-phd.png"), in_reply_to_status_id = get_reply_id())  #


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
my_timeline <- get_my_timeline()  
reply_id <- my_timeline$status_id[1]
post_tweet(tweet.combi.2.tweet,  media = c("data/18_city_new.png",
                                           "data/20_prov_new-no-color.png",
                                           "data/plots/60_routekaart.png",
                                           "data/plots/65_Cases_by_week_facet-grid.png" ), in_reply_to_status_id = reply_id)  #






#### tweet.positive.rate.tweet ####

tweet.positive.rate.tweet <- "Percentage positief en aantal testen."
tweet.positive.rate.tweet <- sprintf(tweet.positive.rate.tweet)
Encoding(tweet.positive.rate.tweet) <- "UTF-8"
my_timeline <- get_my_timeline()  
reply_id <- my_timeline$status_id[1]
post_tweet(tweet.positive.rate.tweet,  media = c("data/plots/22_tests_ggd_daily.png"), in_reply_to_status_id = reply_id)



#### tweet.positive.rate.tweet ####

tweet.positive.age.tweet <- "Aantal besmettingen binnen de leeftijdsgroepen.
Absoluut & per 100.000.
Normale schaal & logaritmisch."
tweet.positive.age.tweet <- sprintf(tweet.positive.age.tweet)
Encoding(tweet.positive.age.tweet) <- "UTF-8"
my_timeline <- get_my_timeline()  
reply_id <- my_timeline$status_id[1]
post_tweet(tweet.positive.age.tweet,  media = c("data/plots/99_leeftijd_case_abs_short.png",
                                                "data/plots/99_leeftijd_case_phd_short.png",
                                                "data/plots/99_leeftijd_case_abs_log.png",
                                                "data/plots/99_leeftijd_case_phd_log.png"), in_reply_to_status_id = reply_id)



