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


 #### New cases EN Tweet ####

diff.cases.day <- abs(a-b)
diff.cases.week <- abs(a-c)

growth.cases.week <- ((e/f)-1)*100
growth.cases.week <- round(growth.cases.week, digits = 0)
doubling.cases.week <- round(log(2)/(log(e/f)/7), digits = 1)


maxValueCase <- max(copy_cases$cases, na.rm = TRUE)
dagRecordCase <- "."


if(a == maxValueCase){
  dagRecordCase <- paste(". (New daily record",intToUtf8(0x26a0), ")",sep = "")
}else {
  dagRecordCase <- "."}

if (a < b) {
  more.less.day.case <- paste("less",intToUtf8(0x2B07), "than yesterday.")
} else if (a > b) {
  more.less.day.case <- paste("more",intToUtf8(0x2197), "than yesterday.")
} else
  more.less.day.case <- paste("more", intToUtf8(0x2194),"than yesterday. (same)")

if (a < c) {
  more.less.week.case <- paste("less",intToUtf8(0x2B07), "than a week ago.")
} else if (a > c) {
  more.less.week.case <- paste("more",intToUtf8(0x2197), "than a week ago.")
} else
  more.less.week.case <- paste("more", intToUtf8(0x2194),"than a week ago. (same)")


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
  doubling.cases.week_text <- paste("halving")
  doubling.cases.week_dot <- intToUtf8(0x1F7E2)
} 
if (doubling.cases.week > 0) {
  doubling.cases.week_text <- paste("doubling")
  doubling.cases.week_dot <- intToUtf8(0x1F534)
} 

doubling.cases.week <- abs(doubling.cases.week)

a  <- format( a, big.mark="." ,decimal.mark=",")
diff.cases.day  <- format( diff.cases.day, big.mark="." ,decimal.mark=",")
diff.cases.week  <- format( diff.cases.week, big.mark="." ,decimal.mark=",")

#### tweet.cases.EN.tweet ####

tweet.cases.EN.tweet <- "New reported cases:

+%s today%s

Indicators (exponential) growth / decay:
%s that is %s %s
%s that is %s %s 

%s --> growth factor: %s%s week on week.
%s --> %s: every %s days."


tweet.cases.EN.tweet <- sprintf(tweet.cases.EN.tweet,
                             a, dagRecordCase,
                             more.less.day.case.dot,  diff.cases.day,    more.less.day.case,
                             more.less.week.case.dot, diff.cases.week,   more.less.week.case,
                             doubling.cases.week_dot, growth.cases.week, deP,
                             doubling.cases.week_dot, doubling.cases.week_text, doubling.cases.week )
Encoding(tweet.cases.EN.tweet) <- "UTF-8"

## post_tweet(tweet.cases.EN.tweet,  media = c("data/05_EN_new_cases.png", "data/05_EN_growth_cases.png", "data/07_EN_cases_type1.png", "data/08_EN_new_cases_WoW.png"))    # "data/06_new_cases_log.png",
 post_tweet(tweet.cases.EN.tweet,  media = c("data/05_EN_new_cases.png", "data/05_EN_growth_cases.png", "data/07_EN_cases_type1.png", "data/08_EN_new_cases_WoW.png"), in_reply_to_status_id = get_reply_id())  #

 #  post_tweet(tweet.cases.EN.tweet)  #

 #### dead EN tweet ####


  diff.dead.day <- abs(n-o)
  diff.dead.week <- abs(n-p)
  maxValuedead <- max(copy_hosp$hosp, na.rm = TRUE)
  dagRecorddead <- "."
  growth.dead.week <- ((w/x)-1)*100
  growth.dead.week <- round(growth.dead.week, digits = 0)
  doubling.dead.week <- round(log(2)/(log(w/x)/7), digits = 1)

if(n == maxValuedead){
  dagRecorddead <- paste(". (New daily record",intToUtf8(0x26a0), ")",sep = "")
}else {
  dagRecorddead <- "."}

if (n < o) {
  more.less.day.dead <- paste("less",intToUtf8(0x2B07), "than yesterday.")
} else if (n > o) {
  more.less.day.dead <- paste("more",intToUtf8(0x2197), "than yesterday.")
} else
  more.less.day.dead <- paste("more", intToUtf8(0x2194),"than yesterday. (same)")

if (n < p) {
  more.less.week.dead <- paste("less",intToUtf8(0x2B07), "than a week ago.")
} else if (n > p) {
  more.less.week.dead <- paste("more",intToUtf8(0x2197), "than a week ago.")
} else
  more.less.week.dead <- paste("more", intToUtf8(0x2194),"than a week ago. (same)")


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
  doubling.dead.week_text <- paste("halving")
  doubling.dead.week_dot <- intToUtf8(0x1F7E2)
} 
if (doubling.dead.week > 0) {
  doubling.dead.week_text <- paste("doubling")
  doubling.dead.week_dot <- intToUtf8(0x1F534)
} 

doubling.dead.week <- abs(doubling.dead.week)
  
n  <- format( n, big.mark="." ,decimal.mark=",")
diff.dead.day  <- format( diff.dead.day, big.mark="." ,decimal.mark=",")
diff.dead.week  <- format( diff.dead.week, big.mark="." ,decimal.mark=",")
  
  #### tweet.dead.EN.tweet ####
  

tweet.dead.EN.tweet <- "New reported deaths:

+%s today%s

Indicators (exponential) growth / decay:
%s that is %s %s
%s that is %s %s

%s --> growth factor: %s%s week on week.
%s --> %s: every %s days."

  tweet.dead.EN.tweet <- sprintf(tweet.dead.EN.tweet,
                            n, dagRecorddead,
                            more.less.day.dead.dot,  diff.dead.day,   more.less.day.dead,
                            more.less.week.dead.dot, diff.dead.week,  more.less.week.dead,
                            doubling.dead.week_dot, growth.dead.week, deP,
                            doubling.dead.week_dot, doubling.dead.week_text, doubling.dead.week)
Encoding(tweet.dead.EN.tweet) <- "UTF-8"
post_tweet(tweet.dead.EN.tweet,  media = c("data/02_EN_leeftijd_heatmap-dead.png","data/13_EN_new_deceased.png", "data/15_EN_dead_diff.png"), in_reply_to_status_id = get_reply_id()) 


#"data/07_new_cases_DoD.png",


#### tweet.age.tweet ####

tweet.age.tweet <- paste("Ages and age distribution of the reported cases.")
post_tweet(status = tweet.age.tweet, 
           media = c("data/01_EN_leeftijd_barchart.png","data/02_EN_leeftijd_heatmap.png", "data/03_EN_leeftijd_relatief.png"), 
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
  dagRecordHosp <- paste(". (New daily record",intToUtf8(0x26a0), ")",sep = "")
}else {
  dagRecordHosp <- "."}

if (h < i) {
  more.less.day.hosp <- paste("less",intToUtf8(0x2B07), "than yesterday.")
} else if (h > i) {
  more.less.day.hosp <- paste("more",intToUtf8(0x2197), "than yesterday.")
} else
  more.less.day.hosp <- paste("more", intToUtf8(0x2194),"than yesterday. (same)")

if (h < j) {
  more.less.week.hosp <- paste("less",intToUtf8(0x2B07), "than a week ago.")
} else if (h > j) {
  more.less.week.hosp <- paste("more",intToUtf8(0x2197), "than a week ago.")
} else
  more.less.week.hosp <- paste("more", intToUtf8(0x2194),"than a week ago. (same)")


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
  doubling.hosp.week_text <- paste("halving")
  doubling.hosp.week_dot <- intToUtf8(0x1F7E2)
} 
if (doubling.hosp.week > 0) {
  doubling.hosp.week_text <- paste("doubling")
  doubling.hosp.week_dot <- intToUtf8(0x1F534)
} 

doubling.hosp.week <- abs(doubling.hosp.week)

h  <- format( h, big.mark="." ,decimal.mark=",")
diff.hosp.day  <- format( diff.hosp.day, big.mark="." ,decimal.mark=",")
diff.hosp.week  <- format( diff.hosp.week, big.mark="." ,decimal.mark=",")

#### tweet.hosp.EN.tweet ####

tweet.hosp.tweet <- "New reported hospitalisations:

+%s today%s

Indicators (exponential) growth / decay:
%s that is %s %s
%s that is %s %s

%s --> growth factor: %s%s week on week.
%s --> %s: every %s days."

tweet.hosp.tweet <- sprintf(tweet.hosp.tweet,
                            h, dagRecordHosp,
                            more.less.day.hosp.dot,  diff.hosp.day,   more.less.day.hosp,
                            more.less.week.hosp.dot, diff.hosp.week,  more.less.week.hosp,
                            doubling.hosp.week_dot, growth.hosp.week,deP,
                            doubling.hosp.week_dot, doubling.hosp.week_text, doubling.hosp.week )
Encoding(tweet.hosp.tweet) <- "UTF-8"
post_tweet(tweet.hosp.tweet,  media = c("data/02_EN_leeftijd_heatmap-hosp.png","data/09_EN_new_hosp.png", "data/05_EN_growth_hosp.png"), in_reply_to_status_id = get_reply_id()) 



#### tweet.carehomes.tweet ####


tweet.carehomes.tweet <- "Verpleeghuizen
"
tweet.carehomes.tweet <- sprintf(tweet.carehomes.tweet)
Encoding(tweet.carehomes.tweet) <- "UTF-8"
post_tweet(tweet.carehomes.tweet,  media = c("data/52_Verpleeg_loc.png", "data/51_Verpleeg_dead.png", "data/50_Verpleeg_cases.png"), in_reply_to_status_id = get_reply_id())  #



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



tweet.growth.tweet <- "Growth Factor week-on-week:

%s cases:   %s %s 
%s hospitalizations (RIVM): %s %s
%s deaths:      %s %s

Growth Factor day-on-day (7-day moving average):
%s cases:   %s %s

Growth Factor day-on-day:
%s cases:   %s %s
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
PersCoSemitwoWeeks = as.Date("2020-11-03",'%Y-%m-%d')
PersCoSemitwoWeeksDays <- as.numeric(difftime(Sys.Date(),PersCoSemitwoWeeks, units = c("days")))
PersCoSemitwoWeeksdone = as.Date("2020-11-17",'%Y-%m-%d')
PersCoSemitwoWeeksdoneDays <- as.numeric(difftime(Sys.Date(),PersCoSemitwoWeeksdone, units = c("days")))
PersColockdown = as.Date("2020-12-14",'%Y-%m-%d')
PersColockdownDays <- as.numeric(difftime(Sys.Date(),PersColockdown, units = c("days")))  

tweet.data.tweet <- "Days since press conference:

[%s] - Bar closes an hour early

[%s] - We want R=0,9

[%s] - semi-lockdown

[%s] - stricter semi-lockdown

[%s] - end of stricter semi-lockdown

[%s] - lockdown

"
#### 17 - press conference: End of 'stricter semi-lockdown'



tweet.data.tweet <- sprintf(tweet.data.tweet,
                            PersCoKroegDays,
                            PersCoPaniekDays,
                            PersCoSemiLockdownDays,
                            PersCoSemitwoWeeksDays,
                            PersCoSemitwoWeeksdoneDays,
                            PersColockdown
)
Encoding(tweet.data.tweet) <- "UTF-8"
post_tweet(tweet.data.tweet, in_reply_to_status_id = get_reply_id()) 


#### tweet.cases.diff.tweet ####


tweet.cases.diff.tweet <- "1) Provinces 
2) Cases:  added today / correctd today.
3) Cases:  compared to yesterday  | Mondays
"
tweet.cases.diff.tweet <- sprintf(tweet.cases.diff.tweet)
Encoding(tweet.cases.diff.tweet) <- "UTF-8"
post_tweet(tweet.cases.diff.tweet,  media = c("data/20_prov_new-test.png", "data/07_EN_cases_diff.png", "data/07_cases_type1-monday.png"), in_reply_to_status_id = get_reply_id())  #

###    media = c("data/17_IC_only.png", "data/16_IC_hosp.png")

#### vakantie tweet EN ####

tweet.vakantie.tweet <- "For people who want to want to compare the region 'North', with regions 'Middle' and 'South'.
Can we draw any conclusions?

No.

(But it's still fun to watch)"

tweet.vakantie.tweet <- sprintf(tweet.vakantie.tweet)
Encoding(tweet.vakantie.tweet) <- "UTF-8"
##  post_tweet(tweet.vakantie.tweet,  media = c("data/40_EN_niet-noord-phd.png"), in_reply_to_status_id = get_reply_id())  #


#### Combi.tweet ####

tweet.combi.2.tweet <- "1) New cases in the 16 big cities
2) New cases in the provinces
3) Routekaart
4) Weekly totals"
tweet.combi.2.tweet <- sprintf(tweet.combi.2.tweet)
Encoding(tweet.combi.2.tweet) <- "UTF-8"
post_tweet(tweet.combi.2.tweet,  media = c("data/18_city_new.png", "data/20_prov_phd.png","data/60_routekaart.png", "data/65_Cases_by_week_facet-grid.png" ), in_reply_to_status_id = get_reply_id())  #




#### 16 cities tweet ####

#tweet.16city.tweet <- "New cases in the 16 big cities"
#tweet.16city.tweet <- sprintf(tweet.16city.tweet)
#Encoding(tweet.16city.tweet) <- "UTF-8"
#post_tweet(tweet.16city.tweet,  media = c("data/18_EN_city_new.png"), in_reply_to_status_id = get_reply_id())

#### provence tweet ####

#tweet.16city.tweet <- "New cases in the provinces"
#tweet.16city.tweet <- sprintf(tweet.16city.tweet)
#Encoding(tweet.16city.tweet) <- "UTF-8"
#post_tweet(tweet.16city.tweet,  media = c("data/20_EN_prov_new.png"), in_reply_to_status_id = get_reply_id()


#### week.tweet ####

tweet.week.tweet <- "The Will-this-week-overtake-last-week-? graph"
tweet.week.tweet <- sprintf(tweet.week.tweet)
Encoding(tweet.week.tweet) <- "UTF-8"
post_tweet(tweet.week.tweet,  media = c("data/65_Cases_by_week.png"), in_reply_to_status_id = get_reply_id())  
