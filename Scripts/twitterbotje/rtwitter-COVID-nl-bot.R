library("readr")
library("rtweet")


## store api keys (these are fake example values; replace with your own keys)
api_key <- "xxxxxxxxxxxxxxxxxxxxxxxxxxx"
api_secret_key <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

## authenticate via web browser
token <- create_token(
  app = "xxxxxxxxxxxxxxxxxxxxxx",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)


CopyGemeenteAantallen <- RIVM_aantallen_gemeente_per_dag


copy_cases <- aggregate(CopyGemeenteAantallen$Total_reported, by=list(dateInTable=CopyGemeenteAantallen$date), FUN=sum)
copy_hosp <- aggregate(CopyGemeenteAantallen$Hospital_admission, by=list(Category=CopyGemeenteAantallen$date), FUN=sum)
copy_dead <- aggregate(CopyGemeenteAantallen$Deceased, by=list(Category=CopyGemeenteAantallen$date), FUN=sum)

colnames(copy_cases) <- c("dateInTable", "cases")
colnames(copy_hosp) <- c("dateInTable", "hosp")
colnames(copy_dead) <- c("dateInTable", "dead")
cases_hosp <- merge(copy_cases,copy_hosp, by.x= "dateInTable")
Merged_data <- merge(copy_dead,cases_hosp, by.x= "dateInTable")

Date <- Sys.Date()
Dateyesterday <- Sys.Date()-1
DateAweekAgo <- Sys.Date()-7

Working_Set <- subset(Merged_data, dateInTable==Date | dateInTable==DateAweekAgo | dateInTable==Dateyesterday )




a <- Working_Set$cases[3]
b <- Working_Set$hosp[3]
c <- Working_Set$dead[3]

deadString<-paste(replicate(c, "☠️"), collapse = "")
hospString<-paste(replicate(1, "🏥"), collapse = "")   #https://www.fileformat.info/info/unicode/char/1f3e5/index.htm


deceased  <- c
inHosp    <- b
newCases  <- a

deceased_text  <- paste(" +", deceased)
inHosp_text    <- paste(" +", inHosp)
newCases_text  <- paste(" +", newCases)


post_tweet(paste("New deceased:\n", deadString, deceased_text, "\n\nNew Hospitalisations:\n",  hospString, inHosp_text,"\n\nNew cases:\n", "😷" , newCases_text))

