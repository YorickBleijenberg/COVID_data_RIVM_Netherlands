





#### tweet.hospital.effect.tweet ####

tweet.hospital.effect.tweet <- "Opnames"

tweet.hospital.effect.tweet <- sprintf(tweet.hospital.effect.tweet)
Encoding(tweet.hospital.effect.tweet) <- "UTF-8"

my_timeline <- get_my_timeline()  
reply_id <- my_timeline$status_id[1]

post_tweet(tweet.hospital.effect.tweet,  media = c("data/plots/70_vaccinated_compare_age_clinic_abs.png",
                                                   "data/plots/16x_omt_check_nice_peak.png",
                                                   # "data/plots/70_vaccinated_compare_age_clinic.png",
                                                   "data/plots/71_vaccinated_compare_age_ICU_abs.png",
                                                   # "data/plots/71_vaccinated_compare_age_IC.png"
                                                   "data/plots/70_vaccinated_compare_age_clinic_abs_young.png" 
), in_reply_to_status_id = reply_id)



