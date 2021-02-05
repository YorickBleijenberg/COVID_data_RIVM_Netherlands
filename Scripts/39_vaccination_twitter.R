##
###
##


### hier komt wat dummy tesx die ik kan debruiken voor later
### en hiet komt nog meer 


start.vaccination.nl = as.Date(c("2021-01-06"))
days.vaccination.in.nl = as.numeric(Sys.Date() - start.vaccination.nl+1)

vaccinated.people.total = 495535
vaccinated.second = 39500+9000
vaccinated.first = vaccinated.people.total-vaccinated.second

vac.perc <-  round((vaccinated.first/17474693*100), digits =4)
vac.perc <- format(vac.perc, scientific=F)
vac.perc.18 <-  round((vaccinated.first/14070340*100), digits =4)
vac.perc.18 <- format(vac.perc.18, scientific=F)

vac.perc.second <-  round((vaccinated.second/17474693*100), digits =4)
vac.perc.second <- format(vac.perc.second, scientific=F)
vac.perc.18.second <-  round((vaccinated.second/14070340*100), digits =4)
vac.perc.18.second <- format(vac.perc.18.second, scientific=F)

freezer = 918480
spillage <- as.integer(vaccinated.people.total/95)*5
in.freezer <- freezer-vaccinated.people.total-spillage

days.vaccination.in.nl
in.freezer
spillage
vaccinated.people.total
vac.perc.18
vac.perc
vac.perc.18.second
vac.perc.second



require(jsonlite)
dat <- fromJSON(txt = "https://coronadashboard.rijksoverheid.nl/json/NL.json")
vaccines_delivery <- as.data.frame(dat$vaccine_delivery$values)
vaccines_delivery$date <- as.Date(as.POSIXct(vaccines_delivery$date_of_insertion_unix, origin="1970-01-01"))

vaccines_used <- as.data.frame(dat$vaccine_delivery$last_value)
vaccines_used$date <- as.Date(as.POSIXct(vaccines_used$date_of_insertion_unix, origin="1970-01-01"))

sum(last(vaccines_used[,c("pfizer","astra_zeneca","cure_vac","janssen","moderna","sanofi")]))

