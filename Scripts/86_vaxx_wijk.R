





wijk_json <- "https://data.rivm.nl/covid-19/COVID-19_vaccinatiegraad_per_wijk_per_week.json"
wijk_dat <- fromJSON(txt = wijk_json)


wijk_dat_short <- wijk_dat[wijk_dat$Date_of_statistics =="2022-01-31",]
