library(tidyverse)
#require(dplyr)
library(zoo)


vacc.effect.age.file <- paste0("https://raw.githubusercontent.com/mzelst/covid-19/master/data-nice/age/leeftijdsverdeling_datum_Klinisch_IC_long.csv")
vacc.effect.age <-read.csv(vacc.effect.age.file,sep=",", check.names = FALSE)
vacc.effect.age$Datum <- as.Date(vacc.effect.age$Datum)

#### select type

type.of.data = "Klinisch"


vacc.effect.age.clin <- (vacc.effect.age  %>% filter(Type == type.of.data & Datum > "2020-12-27")) # & Datum < "2021-02-01"))  #  Klinisch


#### make agegroups ####

# vacc.effect.age.ICU$young = vacc.effect.age.ICU$`20 - 24`+vacc.effect.age.ICU$`25 - 29`+
#  vacc.effect.age.ICU$`30 - 34`+vacc.effect.age.ICU$`35 - 39`+vacc.effect.age.ICU$`40 - 44`+
vacc.effect.age.clin$four   = vacc.effect.age.clin$`40 - 44` + vacc.effect.age.clin$`45 - 49`   #  +vacc.effect.age.ICU$`<20`
vacc.effect.age.clin$five   = vacc.effect.age.clin$`50 - 54` + vacc.effect.age.clin$`55 - 59`
vacc.effect.age.clin$six    = vacc.effect.age.clin$`60 - 64` + vacc.effect.age.clin$`65 - 69`
vacc.effect.age.clin$seven  = vacc.effect.age.clin$`70 - 74` + vacc.effect.age.clin$`75 - 79`
vacc.effect.age.clin$eight  = vacc.effect.age.clin$`80 - 84` + vacc.effect.age.clin$`85 - 89`
vacc.effect.age.clin$nine   = vacc.effect.age.clin$`>90`

#vacc.effect.age.ICU$old85  =  vacc.effect.age.ICU$`85 - 89`
#vacc.effect.age.ICU$old90  =  vacc.effect.age.ICU$`>90`

#vacc.effect.age.ICU <- vacc.effect.age.ICU[ -c(2)]

#### calc increase per day ####

vacc.effect.age.clin <- (vacc.effect.age.clin %>%  mutate(four.Change   = four   - lag(four, default = four[1])) )
vacc.effect.age.clin <- (vacc.effect.age.clin %>%  mutate(five.Change   = five   - lag(five, default = five[1])) )
vacc.effect.age.clin <- (vacc.effect.age.clin %>%  mutate(six.Change    = six    - lag(six, default = six[1])) )
vacc.effect.age.clin <- (vacc.effect.age.clin %>%  mutate(seven.Change  = seven  - lag(seven, default = seven[1])) )
vacc.effect.age.clin <- (vacc.effect.age.clin %>%  mutate(eight.Change  = eight  - lag(eight, default = eight[1])) )
vacc.effect.age.clin <- (vacc.effect.age.clin %>%  mutate(nine.Change   = nine   - lag(nine, default = nine[1])) )

####  MA of the increase ####

vacc.effect.age.clin$MA.four.Change   <- rollmeanr(vacc.effect.age.clin$four.Change  , 7, fill = 0)
vacc.effect.age.clin$MA.five.Change  <- rollmeanr(vacc.effect.age.clin$five.Change , 7, fill = 0)
vacc.effect.age.clin$MA.six.Change   <- rollmeanr(vacc.effect.age.clin$six.Change  , 7, fill = 0)
vacc.effect.age.clin$MA.seven.Change   <- rollmeanr(vacc.effect.age.clin$seven.Change  , 7, fill = 0)
vacc.effect.age.clin$MA.eight.Change  <- rollmeanr(vacc.effect.age.clin$eight.Change , 7, fill = 0)
vacc.effect.age.clin$MA.nine.Change  <- rollmeanr(vacc.effect.age.clin$nine.Change , 7, fill = 0)

#### save file ####

File_date_76 <- paste0("data//plots//clinic_bu.csv")
write.csv2(vacc.effect.age.clin, File_date_76, row.names=FALSE)


#### and ICU data

vacc.effect.age.file <- paste0("https://raw.githubusercontent.com/mzelst/covid-19/master/data-nice/age/leeftijdsverdeling_datum_Klinisch_IC_long.csv")
vacc.effect.age <-read.csv(vacc.effect.age.file,sep=",", check.names = FALSE)
vacc.effect.age$Datum <- as.Date(vacc.effect.age$Datum)

#### select type

type.of.data = "IC"


vacc.effect.age.clin <- (vacc.effect.age  %>% filter(Type == type.of.data & Datum > "2020-12-27"))


#### make agegroups ####


vacc.effect.age.clin$four   = vacc.effect.age.clin$`40 - 44` + vacc.effect.age.clin$`45 - 49` 
vacc.effect.age.clin$five   = vacc.effect.age.clin$`50 - 54` + vacc.effect.age.clin$`55 - 59`
  vacc.effect.age.clin$six    = vacc.effect.age.clin$`60 - 64` + vacc.effect.age.clin$`65 - 69`
vacc.effect.age.clin$seven  = vacc.effect.age.clin$`70 - 74` + vacc.effect.age.clin$`75 - 79`
vacc.effect.age.clin$eight  = vacc.effect.age.clin$`80 - 84` + vacc.effect.age.clin$`85 - 89`
vacc.effect.age.clin$nine   = vacc.effect.age.clin$`>90`

#### calc increase per day ####

vacc.effect.age.clin <- (vacc.effect.age.clin %>%  mutate(four.Change   = four   - lag(four, default = four[1])) )
vacc.effect.age.clin <- (vacc.effect.age.clin %>%  mutate(five.Change   = five   - lag(five, default = five[1])) )
vacc.effect.age.clin <- (vacc.effect.age.clin %>%  mutate(six.Change    = six    - lag(six, default = six[1])) )
vacc.effect.age.clin <- (vacc.effect.age.clin %>%  mutate(seven.Change  = seven  - lag(seven, default = seven[1])) )
vacc.effect.age.clin <- (vacc.effect.age.clin %>%  mutate(eight.Change  = eight  - lag(eight, default = eight[1])) )
vacc.effect.age.clin <- (vacc.effect.age.clin %>%  mutate(nine.Change   = nine   - lag(nine, default = nine[1])) )

####  MA of the increase ####

vacc.effect.age.clin$MA.four.Change   <- rollmeanr(vacc.effect.age.clin$four.Change  , 7, fill = 0)
vacc.effect.age.clin$MA.five.Change  <- rollmeanr(vacc.effect.age.clin$five.Change , 7, fill = 0)
vacc.effect.age.clin$MA.six.Change   <- rollmeanr(vacc.effect.age.clin$six.Change  , 7, fill = 0)
vacc.effect.age.clin$MA.seven.Change   <- rollmeanr(vacc.effect.age.clin$seven.Change  , 7, fill = 0)
vacc.effect.age.clin$MA.eight.Change  <- rollmeanr(vacc.effect.age.clin$eight.Change , 7, fill = 0)
vacc.effect.age.clin$MA.nine.Change  <- rollmeanr(vacc.effect.age.clin$nine.Change , 7, fill = 0)

#### save file ####

File_date_76 <- paste0("data//plots//ICU_bu.csv")
write.csv2(vacc.effect.age.clin, File_date_76, row.names=FALSE)



