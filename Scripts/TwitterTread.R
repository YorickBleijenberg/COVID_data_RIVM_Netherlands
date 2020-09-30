library(tidyverse)
library(rtweet)
library(data.table)


get_reply_id <- function(rel_increase) {
  my_timeline <- get_timeline(rtweet:::home_user()) ## Pull my own tweets
  reply_id <- my_timeline$status_id[1] ## Status ID for reply
  return(reply_id)
}

deE <- intToUtf8(0x00EB)

#today
a <- Working_Set$cases[3]
b <- Working_Set$hosp[3]
c <- Working_Set$dead[3]

#yesterday
n <- Working_Set$cases[2]
o <- Working_Set$hosp[2]
p <- Working_Set$dead[2]

#last week
x <- Working_Set$cases[1]
y <- Working_Set$hosp[1]
z <- Working_Set$dead[1]


diff.cases.day <- abs(a-n)
diff.cases.week <- abs(a-x)
maxValueCase <- max(copy_cases$cases, na.rm = TRUE)
dagRecordCase <- "."


if(a == maxValueCase){
  dagRecordCase <- paste(". (Nieuw dagrecord",intToUtf8(0x26a0), ")",sep = "")
}else {
  dagRecordCase <- "."}

if (a < n) {
 more.less.day.case <- paste("minder",intToUtf8(0x2B07), "dan gisteren.")
} else if (a > n) {
  more.less.day.case <- paste("meer",intToUtf8(0x2197), "dan gisteren.")
} else
  more.less.day.case <- paste("meer", intToUtf8(0x2194),"dan gisteren. (gelijk)")

if (a < x) {
  more.less.week.case <- paste("minder",intToUtf8(0x2B07), "dan een week geleden.")
} else if (a > x) {
  more.less.week.case <- paste("meer",intToUtf8(0x2197), "dan een week geleden.")
} else
  more.less.week.case <- paste("meer", intToUtf8(0x2194),"dan een week geleden. (gelijk)")


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


tweet.cases.tweet <- "Nieuw gemelde besmettingen (zonder correcties):

+%s vandaag%s

Indicatoren exponenti%sle groei / krimp:
%s Dat is %s %s
%s Dat is %s %s"
             
tweet.cases.tweet <- sprintf(tweet.cases.tweet,
                             a, dagRecordCase, deE,
                             more.less.day.case.dot, diff.cases.day,more.less.day.case,
                             more.less.week.case.dot, diff.cases.week, more.less.week.case)
Encoding(tweet.cases.tweet) <- "UTF-8"

 post_tweet(tweet.cases.tweet,  media = c("data/05_new_cases.png","data/06_new_cases_log.png", "data/07_new_cases_DoD.png", "data/08_new_cases_WoW.png")) 
# post_tweet(tweet.cases.tweet,  media = c("data/05_new_cases.png","data/06_new_cases_log.png", "data/07_new_cases_DoD.png", "data/08_new_cases_WoW.png"), in_reply_to_status_id = get_reply_id() ) 








diff.hosp.day <- abs(b-o)
diff.hosp.week <- abs(b-y)
maxValueHosp <- max(copy_hosp$hosp, na.rm = TRUE)
dagRecordHosp <- "."


if(b == maxValueHosp){
  dagRecordHosp <- paste(". (Nieuw dagrecord",intToUtf8(0x26a0), ")",sep = "")
}else {
  dagRecordHosp <- "."}

if (b < o) {
  more.less.day.hosp <- paste("minder",intToUtf8(0x2B07), "dan gisteren.")
} else if (b > o) {
  more.less.day.hosp <- paste("meer",intToUtf8(0x2197), "dan gisteren.")
} else
  more.less.day.hosp <- paste("meer", intToUtf8(0x2194),"dan gisteren. (gelijk)")

if (b < y) {
  more.less.week.hosp <- paste("minder",intToUtf8(0x2B07), "dan een week geleden.")
} else if (b > y) {
  more.less.week.hosp <- paste("meer",intToUtf8(0x2197), "dan een week geleden.")
} else
  more.less.week.hosp <- paste("meer", intToUtf8(0x2194),"dan een week geleden. (gelijk)")


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


tweet.hosp.tweet <- "Nieuw gemelde opnames ziekenhuis (zonder correcties):

+%s vandaag%s

Indicatoren exponenti%sle groei / krimp:
%s Dat is %s %s
%s Dat is %s %s"

tweet.hosp.tweet <- sprintf(tweet.hosp.tweet,
                             b, dagRecordHosp,
                             deE,
                             more.less.day.hosp.dot,  diff.hosp.day,   more.less.day.hosp,
                             more.less.week.hosp.dot, diff.hosp.week,  more.less.week.hosp)
Encoding(tweet.hosp.tweet) <- "UTF-8"
post_tweet(tweet.hosp.tweet,  media = c("data/09_new_hosp.png"), in_reply_to_status_id = get_reply_id()) 











diff.dead.day <- abs(c-p)
diff.dead.week <- abs(c-z)
maxValuedead <- max(copy_hosp$hosp, na.rm = TRUE)
dagRecorddead <- "."


if(c == maxValuedead){
  dagRecorddead <- paste(". (Nieuw dagrecord",intToUtf8(0x26a0), ")",sep = "")
}else {
  dagRecorddead <- "."}

if (c < p) {
  more.less.day.dead <- paste("minder",intToUtf8(0x2B07), "dan gisteren.")
} else if (c > p) {
  more.less.day.dead <- paste("meer",intToUtf8(0x2197), "dan gisteren.")
} else
  more.less.day.dead <- paste("meer", intToUtf8(0x2194),"dan gisteren. (gelijk)")

if (c < z) {
  more.less.week.dead <- paste("minder",intToUtf8(0x2B07), "dan een week geleden.")
} else if (c > z) {
  more.less.week.dead <- paste("meer",intToUtf8(0x2197), "dan een week geleden.")
} else
  more.less.week.dead <- paste("meer", intToUtf8(0x2194),"dan een week geleden. (gelijk)")


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


tweet.dead.tweet <- "Overleden:

+%s vandaag gemeld (excl. corr.)%s

Indicatoren exponenti%sle groei / krimp:
%s Dat is %s %s
%s Dat is %s %s"

tweet.dead.tweet <- sprintf(tweet.dead.tweet,
                            c, dagRecorddead,
                            deE,
                            more.less.day.dead.dot,  diff.dead.day,   more.less.day.dead,
                            more.less.week.dead.dot, diff.dead.week,  more.less.week.dead)
Encoding(tweet.dead.tweet) <- "UTF-8"
post_tweet(tweet.dead.tweet,  media = c("data/13_new_deceased.png"), in_reply_to_status_id = get_reply_id()) 





tweet.age.tweet <- paste("Leeftijden en leeftijdsverdeling gemelde gevallen")
post_tweet(status = tweet.age.tweet, 
           media = c("data/01_leeftijd_barchart.png","data/02_leeftijd_heatmap.png", "data/03_leeftijd_relatief.png"), 
          in_reply_to_status_id = get_reply_id()
           )





