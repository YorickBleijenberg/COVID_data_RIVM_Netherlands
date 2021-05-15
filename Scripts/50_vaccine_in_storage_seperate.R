
## Locale pulled in from the dashboard repository, with most recent version in branch `master`
locale_json <- "https://coronadashboard.rijksoverheid.nl/json/NL.json"
locale_dat <- fromJSON(txt = locale_json)



vacc.deliverd <- locale_dat$vaccine_delivery$values
vacc.deliverd$date_start_unix <-  as.Date(as.POSIXct(vacc.deliverd$date_start_unix , origin="1970-01-01"))
vacc.deliverd$date_of_report_unix  <-  as.Date(as.POSIXct(vacc.deliverd$date_of_report_unix , origin="1970-01-01"))
vacc.deliverd$date_end_unix  <-  as.Date(as.POSIXct(vacc.deliverd$date_end_unix , origin="1970-01-01")) 
vacc.deliverd$date_of_insertion_unix  <-  as.Date(as.POSIXct(vacc.deliverd$date_of_insertion_unix , origin="1970-01-01"))
vacc.deliverd <- vacc.deliverd[ -c(2,3,5)]
colnames(vacc.deliverd) <- c("geleverd", "date")

vacc.expected <- locale_dat$vaccine_delivery_estimate$values
vacc.expected$date_start_unix <-  as.Date(as.POSIXct(vacc.expected$date_start_unix , origin="1970-01-01"))
vacc.expected$date_end_unix  <-  as.Date(as.POSIXct(vacc.expected$date_end_unix , origin="1970-01-01")) 
vacc.expected$date_of_insertion_unix  <-  as.Date(as.POSIXct(vacc.expected$date_of_insertion_unix , origin="1970-01-01")) 
vacc.expected <- vacc.expected[ -c(2,4)]
colnames(vacc.expected) <- c("geleverd", "date")

vacc.total.dash <- rbind(vacc.deliverd,vacc.expected)


vacc.administerd <- locale_dat$vaccine_administered$values
vacc.administerd$date_start_unix <-  as.Date(as.POSIXct(vacc.administerd$date_start_unix , origin="1970-01-01"))
vacc.administerd$date_end_unix  <-  as.Date(as.POSIXct(vacc.administerd$date_end_unix , origin="1970-01-01")) 
vacc.administerd$date_of_insertion_unix  <-  as.Date(as.POSIXct(vacc.administerd$date_of_insertion_unix , origin="1970-01-01")) 
vacc.administerd <- vacc.administerd[ -c(1,8)]
colnames(vacc.administerd) <- c("date", "AstraZeneca","Pfizer","Janssen", "Moderna", "totaal_gezet")

vacc.admin.expect <- locale_dat$vaccine_administered_estimate$values
vacc.admin.expect$date_start_unix <-  as.Date(as.POSIXct(vacc.admin.expect$date_start_unix , origin="1970-01-01"))
vacc.admin.expect$date_end_unix  <-  as.Date(as.POSIXct(vacc.admin.expect$date_end_unix , origin="1970-01-01")) 
vacc.admin.expect$date_of_insertion_unix  <-  as.Date(as.POSIXct(vacc.admin.expect$date_of_insertion_unix , origin="1970-01-01")) 
vacc.admin.expect <- vacc.admin.expect[ -c(1,8)]
colnames(vacc.admin.expect) <- c("date", "AstraZeneca","Pfizer","Janssen", "Moderna", "totaal_gezet")

vacc.total.done.dash <- rbind(vacc.administerd, vacc.admin.expect)

vacc.dash <- merge(vacc.total.dash,vacc.total.done.dash)


vaccination.data.filename <- paste0("data/",format(Sys.Date(), "%Y-%m-%d"),"/",format(Sys.Date(), "%Y-%m-%d"), "_vaccine_deliverd_admin.csv")
write.csv2(vacc.dash, file = vaccination.data.filename, row.names = F)



