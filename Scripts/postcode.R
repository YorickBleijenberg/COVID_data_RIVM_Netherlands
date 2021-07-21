

today = Sys.Date()

poscode_plaats <- read.csv("C:\\Rdir\\data-contstant\\vaccinatie_postcode_plaats.csv",sep=",")  
colnames(poscode_plaats) = c("provincie", "requestCode", "Gemeente_Naam")

postcodes_json <- "data/2021-07-09-postcodes.json"
postcodes_dat <- fromJSON(txt = postcodes_json)

postcodes_dat$date2 <- as.Date(postcodes_dat$dat)
postcodes_dat$today = today
postcodes_dat$days_from_today = postcodes_dat$date2-postcodes_dat$today

overzicht <- merge(postcodes_dat, poscode_plaats, by= "requestCode")




col_order <- c("Gemeente_Naam", "requestCode", "date2",
               "days_from_today", "locationCity", "locationCode")
overzicht2 <- overzicht[, col_order]

colnames(overzicht2) = c("Aanvraag_plaats", "Aanvraag_code", "Eerste_datum_beschikbaar","Over_hoevel_dagen?", "Op_Locatie", "postcode")


overzicht2$`Over_hoevel_dagen?` <- as.integer(overzicht2$`Over_hoevel_dagen?`)

som = sum(overzicht2$`Over_hoevel_dagen?`)
gem =  som/39
komt_uit_op <- today + gem

komt_uit_op

