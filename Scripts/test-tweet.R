


library(rtweet)



## first post
post_tweet(status="first in a thread")

## lookup status_id
my_timeline <- get_my_timeline()  #get_timeline(rtweet:::Yorickb_Graphs)   # Yorickb_Graphs  #get_timeline(rtweet:::home_user())

## ID for reply
reply_id <- my_timeline$status_id[1]

## post reply. this shows up threaded
post_tweet("second in the thread",
           in_reply_to_status_id = reply_id)



#  in_reply_to_status_id = get_reply_id())


my_timeline <- get_my_timeline()  
reply_id <- my_timeline$status_id[1]

in_reply_to_status_id = reply_id