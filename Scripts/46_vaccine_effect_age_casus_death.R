
n= 151

new.date <- Sys.Date()-n
old.date <- new.date-1

old.casus.file <-paste("C:\\Rdir\\rivm-data\\COVID-19_casus_landelijk_", old.date, ".csv", sep = "")
old.casus <- read.csv(old.casus.file,sep=";")
old.casus$Date_statistics <- as.Date(old.casus$Date_statistics)
old.casus = filter(old.casus, Deceased =="Yes")
old.casus = filter(old.casus, Date_statistics > "2020-08-01")

new.casus.file <-paste("C:\\Rdir\\rivm-data\\COVID-19_casus_landelijk_", new.date, ".csv", sep = "")
new.casus <- read.csv(new.casus.file,sep=";")
new.casus$Date_statistics <- as.Date(new.casus$Date_statistics)
new.casus = filter(new.casus, Deceased =="Yes")
new.casus = filter(new.casus, Date_statistics > "2020-08-01")


old.casus.age <- count(old.casus,Agegroup)
new.casus.age <- count(new.casus,Agegroup)

colnames(new.casus.age) <- c("Agegroup","n2")

cassus.age.merge <- merge(old.casus.age,new.casus.age)

cassus.age.merge$diff <- cassus.age.merge$n2-cassus.age.merge$n

sum =  sum(cassus.age.merge$diff)
cassus.age.merge.short <- cassus.age.merge[ -c(2,3)]

date.text = as.character(new.date)
cassus.age.merge.fin <- rbind(c("date",date.text), cassus.age.merge.short)

cassus.age.spread.base <- spread(cassus.age.merge.fin, Agegroup, diff)




n= 150
#n=1

while (n > 0){

new.date <- Sys.Date()-n
old.date <- new.date-1


old.casus.file <-paste("C:\\Rdir\\rivm-data\\COVID-19_casus_landelijk_", old.date, ".csv", sep = "")
old.casus <- read.csv(old.casus.file,sep=";")
old.casus$Date_statistics <- as.Date(old.casus$Date_statistics)
old.casus = filter(old.casus, Deceased =="Yes")
old.casus = filter(old.casus, Date_statistics > "2020-08-01")

new.casus.file <-paste("C:\\Rdir\\rivm-data\\COVID-19_casus_landelijk_", new.date, ".csv", sep = "")
new.casus <- read.csv(new.casus.file,sep=";")
new.casus$Date_statistics <- as.Date(new.casus$Date_statistics)
new.casus = filter(new.casus, Deceased =="Yes")
new.casus = filter(new.casus, Date_statistics > "2020-08-01")

old.casus.age  <-count(old.casus,Agegroup)
new.casus.age  <-count(new.casus,Agegroup)

colnames(new.casus.age) <- c("Agegroup","n2")

cassus.age.merge <- merge(old.casus.age,new.casus.age)

cassus.age.merge$diff <- cassus.age.merge$n2-cassus.age.merge$n

sum =  sum(cassus.age.merge$diff)
cassus.age.merge.short <- cassus.age.merge[ -c(2,3)]

date.text = as.character(new.date)
cassus.age.merge.fin <- rbind(c("date",date.text), cassus.age.merge.short)

cassus.age.spread <- spread(cassus.age.merge.fin, Agegroup, diff)

cassus.age.spread.base <- rbind(cassus.age.spread.base, cassus.age.spread)


n <- n-1

}
 


#cassus.age.spread.base  <- read.csv("C:\\Rdir\\data-contstant\\casus_daily_age_dif_death.csv" ,sep=";", check.names = FALSE)


File_date_5 <- paste0("data/casus_daily_age_diff_death.csv")

write.csv2(cassus.age.spread.base, File_date_5, row.names=FALSE)





