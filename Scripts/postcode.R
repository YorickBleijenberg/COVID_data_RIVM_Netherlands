


library(ggmap)




today = Sys.Date()

poscode_plaats <- read.csv("C:\\Rdir\\data-contstant\\vaccinatie_postcode_plaats.csv",sep=",")  
colnames(poscode_plaats) = c("provincie", "requestCode", "Gemeente_Naam", "from")

postcodes_json <- "data/2021-12-30_vaccinatie_postcode_plaats.json"
postcodes_dat <- fromJSON(txt = postcodes_json)

postcodes_dat$date2 <- as.Date(postcodes_dat$dat)
postcodes_dat$today = today
postcodes_dat$days_from_today = postcodes_dat$date2-postcodes_dat$today

overzicht <- merge(postcodes_dat, poscode_plaats, by= "requestCode")

overzicht$locationCode <- str_replace_all(overzicht$locationCode," ", "")




col_order <- c("Gemeente_Naam", "requestCode", "locationCity","days_from_today",
               "date2","date", "locationCode","today","provincie","from")
overzicht <- overzicht[, col_order]

overzicht <- overzicht[order(overzicht$days_from_today),]  


File_date_5cdx <- paste0("data/", format(Sys.time(), "%Y-%m-%d"),"_ggd_locaties.csv")
write.csv2(overzicht, File_date_5cdx, row.names=FALSE) 



register_google(key = "AIzaSyAueo2UQ08Az63qx_ArJkIBjpff78R3wkU")






overzicht_working <- overzicht

overzicht_working$row.number <- 1:nrow(overzicht_working)  


overzicht_working$locationCode <- str_replace_all(overzicht_working$locationCode, "9713WS", "9712KZ")
overzicht_working$locationCode <- str_replace_all(overzicht_working$locationCode, "2402NZ", "2402ED")
overzicht_working$locationCode <- str_replace_all(overzicht_working$locationCode, "3084BA", "3084BB")
overzicht_working$locationCode <- str_replace_all(overzicht_working$locationCode, "4817ZX", "4817ZN")
overzicht_working$locationCode <- str_replace_all(overzicht_working$locationCode, "6001HZ", "6001GD")
overzicht_working$locationCode <- str_replace_all(overzicht_working$locationCode, "3841WD", "3841BJ")
overzicht_working$locationCode <- str_replace_all(overzicht_working$locationCode, "7203AA", "7203AD")
overzicht_working$locationCode <- str_replace_all(overzicht_working$locationCode, "1406NW", "1406NZ")




for (i in overzicht_working$row.number){
  orig <- overzicht_working[i,c('from')] # get origin from DF in the position line 'i', column 'from'
  dest <- overzicht_working[i,c('locationCode')]   # get origin from DF in the position line 'i', column 'to'
  a <- mapdist(from = orig, to = dest, mode = "driving",output = "simple") # create temp. df 'a' with the output from mapdist
  a$row.number <- i # include in temp. df 'a' the index number of the row from DF
  overzicht_working$km[match(a$row.number, overzicht_working$row.number)] <- a$km # ibdem
  overzicht_working$minutes[match(a$row.number, overzicht_working$row.number)] <- a$minutes # use the index number as a matching key to input/update the value of the variable 'minutes'
}


for (i in overzicht_working$row.number){
  orig <- overzicht_working[i,c('from')] # get origin from DF in the position line 'i', column 'from'
  dest <- overzicht_working[i,c('locationCode')]   # get origin from DF in the position line 'i', column 'to'
  a <- mapdist(from = orig, to = dest, mode = "bicycling",output = "simple") # create temp. df 'a' with the output from mapdist
  a$row.number <- i # include in temp. df 'a' the index number of the row from DF
  overzicht_working$minutes_bike[match(a$row.number, overzicht_working$row.number)] <- a$minutes # use the index number as a matching key to input/update the value of the variable 'minutes'
}


overzicht_working <- overzicht_working[, -c(5:11)]

overzicht_working$km <- round(overzicht_working$km,0)
overzicht_working$minutes <- round(overzicht_working$minutes,0)
overzicht_working$minutes_bike <- round(overzicht_working$minutes_bike,0)




File_date_5cdxyz <- paste0("data/", format(Sys.time(), "%Y-%m-%d"),"_ggd_locaties_driving.csv")
write.csv2(overzicht_working, File_date_5cdxyz, row.names=FALSE) 











lastRowForoGoogle <- last(overzicht_working,2)
overzicht_working <- head(overzicht_working,-2)

overzicht_km_google <-1
overzicht_km_google <- mapdist(lastRowForoGoogle$from,lastRowForoGoogle$locationCode, output = c("simple") ) #   ,mode = c( "transit"))
overzicht_merge <- merge(overzicht, overzicht_km_google, by= "from")

GoogleRowCount = 50
# while (GoogleRowCount > 1) {
  lastRowForoGoogle <- last(overzicht_working,2)
  overzicht_km_google <-1
  overzicht_km_google <- mapdist(lastRowForoGoogle$from,lastRowForoGoogle$locationCode ) #   ,mode = c( "transit"))
  overzicht_merge_loop <- merge(overzicht, overzicht_km_google, by= "from")
  overzicht_merge <- rbind(overzicht_merge, overzicht_merge_loop)
  overzicht_working <- head(overzicht_working,-2)
  GoogleRowCount = GoogleRowCount -1
#  }

overzicht_working <- overzicht

lastRowForoGoogle <- last(overzicht_working,2)
overzicht_working <- head(overzicht_working,-2)

overzicht_km_google <-1
overzicht_km_google <- mapdist(lastRowForoGoogle$from,lastRowForoGoogle$locationCode, mode = c( "bicycling") ) #   ,mode = c( "transit"))
overzicht_merge_bike <- merge(overzicht, overzicht_km_google, by= "from")

GoogleRowCount = 50
while (GoogleRowCount > 1) {
  lastRowForoGoogle <- last(overzicht_working,2)
  overzicht_km_google <- mapdist(lastRowForoGoogle$from,lastRowForoGoogle$locationCode, mode = c( "bicycling"))
  overzicht_merge_loop <- merge(overzicht, overzicht_km_google, by= "from")
  overzicht_merge_bike <- rbind(overzicht_merge_bike, overzicht_merge_loop)
  overzicht_working <- head(overzicht_working,-2)
  GoogleRowCount = GoogleRowCount -1
}


overzicht_merge_compact <- overzicht_merge
overzicht_merge_compact <- overzicht_merge_compact[,-c(1, 6:12, 14,15, 17,18)]
overzicht_merge_compact <- overzicht_merge_compact[order(overzicht_merge_compact$minutes),]  

gem_afstand=    round(  (sum(overzicht_merge_compact$km)/39)   ,1)
gem_tijd_auto = round(  (sum(overzicht_merge_compact$minutes)/39)  ,1)


overzicht_merge_compact$minutes <- round(overzicht_merge_compact$minutes,0)
overzicht_merge_compact$km <- round(overzicht_merge_compact$km,1)

overzicht_merge_bike_compact <- overzicht_merge_bike
overzicht_merge_bike_compact$minutes_bike <- round(overzicht_merge_bike_compact$minutes,0)
overzicht_merge_bike_compact <- overzicht_merge_bike_compact[, -c(1,2,4:18)]

overzicht_merge_compact <- merge(overzicht_merge_compact,overzicht_merge_bike_compact, by="requestCode")

overzicht_merge_compact <- merge(overzicht,overzicht_merge_compact, by="requestCode")

overzicht_merge_compact <- overzicht_merge_compact[, -c(5:13)]



gem_afstand
gem_tijd_auto


overzicht_merge_compact <- overzicht_merge_compact[order(overzicht_merge_compact$days_from_today),]  










col_order <- c("Gemeente_Naam", "requestCode", "date2",
               "days_from_today", "locationCity", "locationCode")
overzicht2 <- overzicht[, col_order]
colnames(overzicht2) = c("Aanvraag_plaats", "Aanvraag_code", "Eerste_datum_beschikbaar","Over_hoevel_dagen?", "Op_Locatie", "postcode")
overzicht2$`Over_hoevel_dagen?` <- as.integer(overzicht2$`Over_hoevel_dagen?`)
som = sum(overzicht2$`Over_hoevel_dagen?`)
gem =  som/39
komt_uit_op <- today + gem
komt_uit_op




overzicht45 <- overzicht_merge
overzicht45 <- overzicht45[,-c(1,2,4,6,7,9,11,12,14,15,17,18)] 


overzicht47 <-overzicht45
colnames(overzicht47) = c("a", "b", "c","d?", "e", "f","g","h","i")

overzicht47 <- overzicht47[,-c(4,5,6)] 



