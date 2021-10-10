

####  IC opname afteller ####


OMTmin = 2200
OMTmax = 3400


LCPS_OMT_Feed<-read.csv("https://lcps.nu/wp-content/uploads/covid-19-datafeed.csv",sep=",")  
LCPS_OMT_Feed$Datum <- as.Date(LCPS_OMT_Feed$Datum ,format="%d-%m-%Y")

LCPS_OMT_Feed <- LCPS_OMT_Feed [, -c(2,3,4,6)]  

LCPS_OMT_Feed_sept <-  (LCPS_OMT_Feed %>% filter(Datum > "2021-09-15" ))

Sum_IC_since_sept <- sum(LCPS_OMT_Feed_sept$IC_Nieuwe_Opnames_COVID)



OMT_min_to_go <- OMTmin - Sum_IC_since_sept
OMT_max_to_go <- OMTmax - Sum_IC_since_sept

OMT_min_to_go <-  format(OMT_min_to_go ,big.mark = ".", decimal.mark = ",")
OMT_max_to_go <-  format(OMT_max_to_go ,big.mark = ".", decimal.mark = ",")


#### tweet.OMT.To.Go.tweet ####

tweet.OMT.To.Go.tweet <- "RIVM: we verwachten nog 2.200 tot 3.400 IC opnames vanaf half september tot ... wanneer corona voorbij is.

Aantal IC opnames sinds 15 september:
[%s]

Dus, aantal nog te gaan:
[%s - %s]
"
tweet.OMT.To.Go.tweet <- sprintf(tweet.OMT.To.Go.tweet,
                                 Sum_IC_since_sept,
                                 OMT_min_to_go,
                                 OMT_max_to_go
)
Encoding(tweet.OMT.To.Go.tweet) <- "UTF-8"
post_tweet(tweet.OMT.To.Go.tweet, in_reply_to_status_id = get_reply_id())
