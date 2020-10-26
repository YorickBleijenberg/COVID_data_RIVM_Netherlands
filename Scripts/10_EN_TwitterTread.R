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

#today
a <- Working_Set$cases[3]
b <- Working_Set$hosp[3]
c <- Working_Set$dead[3]
e <- Working_Set$MACases[3]

#yesterday
n <- Working_Set$cases[2]
o <- Working_Set$hosp[2]
p <- Working_Set$dead[2]

#last week
x <- Working_Set$cases[1]
y <- Working_Set$hosp[1]
z <- Working_Set$dead[1]
f <- Working_Set$MACases[1]


diff.cases.day <- abs(a-n)
diff.cases.week <- abs(a-x)
growth.cases.week <- ((e/f)-1)*100
growth.cases.week <- round(growth.cases.week, digits = 0)
doubling.cases.week <- round(log(2)/(log(e/f)/7), digits = 1)

maxValueCase <- max(copy_cases$cases, na.rm = TRUE)
dagRecordCase <- "."

if(a == maxValueCase){
  dagRecordCase <- paste(". (New daily record",intToUtf8(0x26a0), ")",sep = "")
}else {
  dagRecordCase <- "."}

if (a < n) {
  more.less.day.case <- paste("less",intToUtf8(0x2B07), "than yesterday.")
} else if (a > n) {
  more.less.day.case <- paste("more",intToUtf8(0x2197), "than yesterday.")
} else
  more.less.day.case <- paste("more", intToUtf8(0x2194),"than yesterday. (same)")

if (a < x) {
  more.less.week.case <- paste("less",intToUtf8(0x2B07), "than a week ago.")
} else if (a > x) {
  more.less.week.case <- paste("more",intToUtf8(0x2197), "than a week ago.")
} else
  more.less.week.case <- paste("more", intToUtf8(0x2194),"than a week ago. (same)")


if (a < n) {
  more.less.day.case.dot <- intToUtf8(0x1F7E2)
} else if (a > n) {
  more.less.day.case.dot <- intToUtf8(0x1F534)
} else
  more.less.day.case.dot <- intToUtf8(0x1F7E1)

if (a < x) {
  more.less.week.case.dot <- intToUtf8(0x1F7E2)
} else if (a > x) {
  more.less.week.case.dot <- intToUtf8(0x1F534)
} else
  more.less.week.case.dot <- intToUtf8(0x1F7E1)

if (doubling.cases.week < 0) {
  doubling.cases.week_text <- paste("half time")
} 
if (doubling.cases.week > 0) {
  doubling.cases.week_text <- paste("doubling")
} 


tweet.cases.tweet <- "New reported cases:

+%s today%s

Indicators (exponential) growth / decay:
%s that is %s %s
%s that is %s %s 

%s --> growth factor: %s%s week on week.
%s --> %s: every %s days."


tweet.cases.tweet <- sprintf(tweet.cases.tweet,
                             a, dagRecordCase,
                             more.less.day.case.dot, diff.cases.day,more.less.day.case,
                             more.less.week.case.dot, diff.cases.week, more.less.week.case,
                             more.less.week.case.dot, growth.cases.week,deP,
                             more.less.week.case.dot, doubling.cases.week_text, doubling.cases.week )
Encoding(tweet.cases.tweet) <- "UTF-8"

#   post_tweet(tweet.cases.tweet,  media = c("data/05_new_cases.png", "data/05_growth_cases.png", "data/07_cases_type1.png", "data/08_new_cases_WoW.png"))    # "data/06_new_cases_log.png",
post_tweet(tweet.cases.tweet,  media = c("data/05_EN_new_cases.png", "data/05_EN_growth_cases.png", "data/07_EN_cases_type1.png", "data/08_EN_new_cases_WoW.png"), in_reply_to_status_id = get_reply_id())  #

 #  post_tweet(tweet.cases.tweet)  #




diff.dead.day <- abs(c-p)
diff.dead.week <- abs(c-z)
maxValuedead <- max(copy_hosp$hosp, na.rm = TRUE)
dagRecorddead <- "."


if(c == maxValuedead){
  dagRecorddead <- paste(". (New daily record",intToUtf8(0x26a0), ")",sep = "")
}else {
  dagRecorddead <- "."}

if (c < p) {
  more.less.day.dead <- paste("less",intToUtf8(0x2B07), "than yesterday.")
} else if (c > p) {
  more.less.day.dead <- paste("more",intToUtf8(0x2197), "than yesterday.")
} else
  more.less.day.dead <- paste("more", intToUtf8(0x2194),"than yesterday. (same)")

if (c < z) {
  more.less.week.dead <- paste("less",intToUtf8(0x2B07), "than a week ago.")
} else if (c > z) {
  more.less.week.dead <- paste("more",intToUtf8(0x2197), "than a week ago.")
} else
  more.less.week.dead <- paste("more", intToUtf8(0x2194),"than a week ago. (same)")


if (c < p) {
  more.less.day.dead.dot <- intToUtf8(0x1F7E2)
} else if (c > p) {
  more.less.day.dead.dot <- intToUtf8(0x1F534)
} else
  more.less.day.dead.dot <- intToUtf8(0x1F7E1)

if (c < z) {
  more.less.week.dead.dot <- intToUtf8(0x1F7E2)
} else if (c > z) {
  more.less.week.dead.dot <- intToUtf8(0x1F534)
} else
  more.less.week.dead.dot <- intToUtf8(0x1F7E1)


tweet.dead.tweet <- "New reported deaths:

+%s today%s

Indicators (exponential) growth / decay:
%s that is %s %s
%s that is %s %s"

tweet.dead.tweet <- sprintf(tweet.dead.tweet,
                            c, dagRecorddead,
                            more.less.day.dead.dot,  diff.dead.day,   more.less.day.dead,
                            more.less.week.dead.dot, diff.dead.week,  more.less.week.dead)
Encoding(tweet.dead.tweet) <- "UTF-8"
post_tweet(tweet.dead.tweet,  media = c("data/02_EN_leeftijd_heatmap-dead.png","data/13_EN_new_deceased.png", "data/15_EN_dead_diff.png"), in_reply_to_status_id = get_reply_id()) 




#"data/07_new_cases_DoD.png",


diff.hosp.day <- abs(b-o)
diff.hosp.week <- abs(b-y)
maxValueHosp <- max(copy_hosp$hosp, na.rm = TRUE)
dagRecordHosp <- "."


if(b == maxValueHosp){
  dagRecordHosp <- paste(". (New daily record",intToUtf8(0x26a0), ")",sep = "")
}else {
  dagRecordHosp <- "."}

if (b < o) {
  more.less.day.hosp <- paste("less",intToUtf8(0x2B07), "than yesterday.")
} else if (b > o) {
  more.less.day.hosp <- paste("more",intToUtf8(0x2197), "than yesterday.")
} else
  more.less.day.hosp <- paste("more", intToUtf8(0x2194),"than yesterday. (same)")

if (b < y) {
  more.less.week.hosp <- paste("less",intToUtf8(0x2B07), "than a week ago.")
} else if (b > y) {
  more.less.week.hosp <- paste("more",intToUtf8(0x2197), "than a week ago.")
} else
  more.less.week.hosp <- paste("more", intToUtf8(0x2194),"than a week ago. (same)")


if (b < o) {
  more.less.day.hosp.dot <- intToUtf8(0x1F7E2)
} else if (b > o) {
  more.less.day.hosp.dot <- intToUtf8(0x1F534)
} else
  more.less.day.hosp.dot <- intToUtf8(0x1F7E1)

if (b < y) {
  more.less.week.hosp.dot <- intToUtf8(0x1F7E2)
} else if (b > y) {
  more.less.week.hosp.dot <- intToUtf8(0x1F534)
} else
  more.less.week.hosp.dot <- intToUtf8(0x1F7E1)


tweet.hosp.tweet <- "New reported hospitalisations:

+%s today%s

Indicators (exponential) growth / decay:
%s that is %s %s
%s that is %s %s"

tweet.hosp.tweet <- sprintf(tweet.hosp.tweet,
                            b, dagRecordHosp,
                            more.less.day.hosp.dot,  diff.hosp.day,   more.less.day.hosp,
                            more.less.week.hosp.dot, diff.hosp.week,  more.less.week.hosp)
Encoding(tweet.hosp.tweet) <- "UTF-8"
post_tweet(tweet.hosp.tweet,  media = c("data/02_EN_leeftijd_heatmap-hosp.png","data/09_EN_new_hosp.png", "data/05_EN_growth_hosp.png"), in_reply_to_status_id = get_reply_id()) 












tweet.age.tweet <- paste("Ages and age distribution of the reported cases.")
post_tweet(status = tweet.age.tweet, 
           media = c("data/01_EN_leeftijd_barchart.png","data/02_EN_leeftijd_heatmap.png", "data/03_EN_leeftijd_relatief.png"), 
           in_reply_to_status_id = get_reply_id()
)









#tweet.growth.tweet <- "Groei week op week:

#besmettingen:  %s %s 
#opnames:        %s %s
#overleden:      %s %s"

#tweet.growth.tweet <- sprintf(tweet.growth.tweet,
#                              gf_c_last,deP,
#                              gf_h_last,deP,
#                              gf_d_last,deP)
#   Encoding(tweet.growth.tweet) <- "UTF-8"
#    post_tweet(tweet.growth.tweet,  media = c("data/05_growth_cases.png", "data/05_growth_hosp.png","data/05_growth_dead.png"), in_reply_to_status_id = get_reply_id()) 










PersCoKroeg = as.Date("2020-09-18",'%Y-%m-%d')
PersCoKroegDays <- as.numeric(difftime(Sys.Date(),PersCoKroeg, units = c("days")))
PersCoPaniek = as.Date("2020-09-28",'%Y-%m-%d')
PersCoPaniekDays <- as.numeric(difftime(Sys.Date(),PersCoPaniek, units = c("days")))   
PersCoSemiLockdown = as.Date("2020-10-13",'%Y-%m-%d')
PersCoSemiLockdownDays <- as.numeric(difftime(Sys.Date(),PersCoSemiLockdown, units = c("days")))  

tweet.data.tweet <- "Days since:

%s - press conference: 'Bar closes an hour early'

%s - press conference: 'We want R=0,9'

%s  - press conference: 'semi-lockdown'"


tweet.data.tweet <- sprintf(tweet.data.tweet,
                            PersCoKroegDays,PersCoPaniekDays,PersCoSemiLockdownDays
)
Encoding(tweet.data.tweet) <- "UTF-8"
post_tweet(tweet.data.tweet, in_reply_to_status_id = get_reply_id()) 



tweet.cases.diff.tweet <- "1) New cases:  difference compared to yesterday.
2) mondays
3) Number of people on the ICU
4) number of people in the hospital with COVID-19"
tweet.cases.diff.tweet <- sprintf(tweet.cases.diff.tweet)
Encoding(tweet.cases.diff.tweet) <- "UTF-8"
post_tweet(tweet.cases.diff.tweet,  media = c("data/07_cases_diff.png", "data/07_cases_type1-monday.png", "data/17_IC_only.png", "data/16_IC_hosp.png"), in_reply_to_status_id = get_reply_id())


#### provence tweet ####

tweet.16city.tweet <- "New cases in the provences"
tweet.16city.tweet <- sprintf(tweet.16city.tweet)
Encoding(tweet.16city.tweet) <- "UTF-8"
post_tweet(tweet.16city.tweet,  media = c("data/20_EN_prov_new.png"), in_reply_to_status_id = get_reply_id())  #




#### 16 cities tweet ####

tweet.16city.tweet <- "New cases in the 16 big cities"
tweet.16city.tweet <- sprintf(tweet.16city.tweet)
Encoding(tweet.16city.tweet) <- "UTF-8"
post_tweet(tweet.16city.tweet,  media = c("data/18_EN_city_new.png"), in_reply_to_status_id = get_reply_id())  #





