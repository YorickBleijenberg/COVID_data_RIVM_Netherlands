


vac.data.cities <- read.csv("C:\\Rdir\\data\\vacdata\\04082021_vaccinatiegraad.csv",sep=",")
colnames(vac.data.cities) = c("vac.status","geboortejaar","Gemeente_Naam", "opkomst")
vac.data.cities$geboortejaar <- as.factor(vac.data.cities$geboortejaar)
vac.data.cities$opkomst <- as.factor(vac.data.cities$opkomst)
vac.data.cities$vac.status <- as.factor(vac.data.cities$vac.status)




inwo_gem <- "C:\\Rdir\\data-contstant\\CBS_inwoners_gemeente.csv"
gemeente.inwoners <- read.csv(inwo_gem,sep=";")  
colnames(gemeente.inwoners) = c("Municipality_code", "Gemeente_Naam", "inwoners", "gemeente_getal")

FR_a <- intToUtf8(0x00E2)  #Encoding(FR_b) <- "UTF-8"
FR_u <- intToUtf8(0x00FA)  #Encoding(FR_b) <- "UTF-8"
SFR_name <- paste0("S", FR_u, "dwest Frysl", FR_a,"n")
SFR_name2 <- paste0("Noardeast-Frysl", FR_a,"n")
gemeente.inwoners$Gemeente_Naam <- str_replace(gemeente.inwoners$Gemeente_Naam, "Súdwest Fryslân", SFR_name)  ##fout / goed
gemeente.inwoners$Gemeente_Naam <- str_replace(gemeente.inwoners$Gemeente_Naam, "Noardeast-Fryslân", SFR_name2)  ##fout / goed



vac.cities.combi <- merge(vac.data.cities,gemeente.inwoners)
#vac.cities.combi$Gemeente_Naam <- as.factor(vac.cities.combi$Gemeente_Naam)

vac.cities.combi.filter <- vac.cities.combi

vac.cities.combi.filter <- vac.cities.combi.filter[vac.cities.combi.filter$vac.status == "Volledig gevaccineerd" & vac.cities.combi.filter$geboortejaar == "Alle volwassenen" ,]
