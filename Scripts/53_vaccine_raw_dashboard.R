


#### dashboard raw update



locale_json <- "https://coronadashboard.rijksoverheid.nl/json/NL.json"
locale_dat <- fromJSON(txt = locale_json)

as.Date(as.POSIXct(locale_dat$vaccine_administered_total$last_value$date_unix , origin="1970-01-01"))
locale_dat$vaccine_administered_total$last_value$estimated
locale_dat$vaccine_administered_total$last_value$reported


first.file <- locale_dat$vaccine_administered_total$values
first.file$date_unix <-as.Date(as.POSIXct(first.file$date_unix , origin="1970-01-01"))

vac.given.filename <- paste0("data/estimated.csv")
write.csv2(first.file, file = vac.given.filename, row.names = F)



second.file <- locale_dat$vaccine_administered_ggd_ghor$values
second.file$date_unix <-as.Date(as.POSIXct(second.file$date_unix , origin="1970-01-01"))

vac.given.filename <- paste0("data/ggd.csv")
write.csv2(second.file, file = vac.given.filename, row.names = F)



third.file <- locale_dat$vaccine_administered_doctors$values


