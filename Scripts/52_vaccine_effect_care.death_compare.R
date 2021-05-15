
#  https://raw.githubusercontent.com/mzelst/covid-19/master/data-rivm/nursing-homes-per-day/nursery_daily_2021-04-27.csv

#https://raw.githubusercontent.com/mzelst/covid-19/master/data-rivm/nursing-homes-per-day/nursery_daily_2021-04-27.csv

require(data.table)

#dt.old = fread("https://raw.githubusercontent.com/mzelst/covid-19/master/data-rivm/nursing-homes-per-day/nursery_daily_2021-04-22.csv")
# dt.new = fread("https://raw.githubusercontent.com/mzelst/covid-19/master/data-rivm/nursing-homes-per-day/nursery_daily_2021-04-27.csv")
#write.csv2(dt.old, file = "data/disabled.old.v.csv",row.names = F)
#write.csv2(dt.new, file = "data/disabled.new.csv",row.names = F)

n= 171

while (n > 0){
 

new.date <- Sys.Date()-n
old.date <- new.date-1

# old.disabled.file <- paste0("https://raw.githubusercontent.com/mzelst/covid-19/master/data-rivm/nursing-homes-per-day/nursery_daily_", old.date, ".csv", sep = "")
# old.casus <- fread(old.disabled.file)
# old.casus$date <- as.Date(old.casus$date)
# old.casus <- old.casus[,-c(2,4:6)]

new.disabled.file <- paste0("https://raw.githubusercontent.com/mzelst/covid-19/master/data-rivm/nursing-homes-per-day/nursery_daily_", new.date, ".csv", sep = "")
new.casus <- fread(new.disabled.file)
new.casus$date <- as.Date(new.casus$date)
new.casus <- new.casus[,-c(2,4:6)]

#### calulate differenc between yesterday and today
# old.total.cases.disabled     <- sum(old.casus$Total_cases_reported)
# old.total.deceased.disabled  <- sum(old.casus$Total_deceased_reported)
# new.total.cases.disabled     <- sum(new.casus$Total_cases_reported)
# new.total.deceased.disabled  <- sum(new.casus$Total_deceased_reported)
# diff.cases.disabled     <-  new.total.cases.disabled    - old.total.cases.disabled
# diff.deceased.disabled  <-  new.total.deceased.disabled - old.total.deceased.disabled

### create new DF. ####
#new.line <- setNames(data.frame(matrix(ncol = 3, nrow = 1)), c("date", "new_cases", "new_dead"))
#new.line.two <- c(as.character(new.date), diff.cases.disabled,diff.deceased.disabled)

 #  cassus.age.merge.fin <- rbind(old.casus, new.casus)

cassus.age.merge.fin <- rbind(cassus.age.merge.fin, new.casus)

#cassus.age.merge.fin = cassus.age.merge.fin[-1,]

n <- n-1

}

##cassus.age.spread.base  <- read.csv("C:\\Rdir\\data-contstant\\casus_daily_age_dif.csv" ,sep=";", check.names = FALSE)



File_date_72 <- paste0("data//", Sys.Date()-1 , "/",Sys.Date()-1, "_care_daily_diff.csv")
write.csv(cassus.age.merge.fin, File_date_72, row.names=FALSE)


casus.care.merge.fin <- cassus.age.merge.fin







