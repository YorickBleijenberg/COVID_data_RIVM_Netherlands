
library(tidyverse)

library(lubridate)


trash.icon1 <- intToUtf8(0x1F5D1)
trash.icon2 <- intToUtf8(0x1F6AE)

vaccin.exp  = as.Date("2021-08-31",'%Y-%m-%d')
today  <- Sys.Date()
prullenbak.dagen<- as.vector(difftime(vaccin.exp, today, units='days'))


tweet.prullenbak.tweet <- "

Het aantal dagen totdat de huisartsen van Hugo de (nu nog goede) Astrazeneca vaccins in de prullenbak moeten gooien %s%s:

[%s]

Draadje: (https://twitter.com/YorickB/status/1424586458183127042)

#prullenbakvaccin
 
"

tweet.prullenbak.tweet <- sprintf(  tweet.prullenbak.tweet,
                                    trash.icon2, trash.icon1,
                                    prullenbak.dagen
)

Encoding(tweet.prullenbak.tweet) <- "UTF-8"
post_tweet(tweet.prullenbak.tweet, in_reply_to_status_id = get_reply_id())  #,  media = c("data/belgische_hokjes.png"), in_reply_to_status_id = get_reply_id())
