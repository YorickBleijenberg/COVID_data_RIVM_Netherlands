


case_predict.file <-paste0("C:\\Rdir\\data\\",Sys.Date(),"\\", Sys.Date(),"_casus_daily_age_dif.csv")
case_predict <- read.csv(case_predict.file,sep=";")
case_predict <- case_predict[,-c(1,13)]
colnames(case_predict) <- c("very_young","young", "twenty", "thirty", "forthy", "fithy", "sixty", "seventy", "old","very_old", "date")
case_predict$young = case_predict$very_young+case_predict$young
case_predict$old = case_predict$very_old+case_predict$old
case_predict <- case_predict[,-c(1,10)]
case_predict$date = as.Date(case_predict$date)

case_predict <- case_predict[case_predict$date>"2021-03-20",]


#0.001350044
#0.002003394
#30 - 0.006444603
#0.009787346
#0.025945658
#60 - 0.038038507
#0.102677075
#0.169386085
#0.101264418

#case_predict$young     *0.005
#case_predict$twenty    *0.005
#case_predict$thirty    *0.015
#case_predict$forthy    *0.025
#case_predict$fithy     *0.05
#case_predict$sixty     *0.1
#case_predict$seventy   *0.125
#case_predict$old       *0.2


#case_predict$young_hosp_pred   = case_predict$young     *0.005
#case_predict$twenty_hosp_pred  = case_predict$twenty    *0.005
#case_predict$thirty_hosp_pred  = case_predict$thirty    *0.010
#case_predict$forthy_hosp_pred  = case_predict$forthy    *0.017
#case_predict$fithy_hosp_pred   = case_predict$fithy     *0.025
#case_predict$sixty_hosp_pred   = case_predict$sixty     *0.05
#case_predict$seventy_hosp_pred = case_predict$seventy   *0.120
#case_predict$old_hosp_pred     = case_predict$old       *0.2

#case_predict$young_hosp_pred   = case_predict$young     *0.004
#case_predict$twenty_hosp_pred  = case_predict$twenty    *0.0045
#case_predict$thirty_hosp_pred  = case_predict$thirty    *0.006
#case_predict$forthy_hosp_pred  = case_predict$forthy    *0.01
#case_predict$fithy_hosp_pred   = case_predict$fithy     *0.012
#case_predict$sixty_hosp_pred   = case_predict$sixty     *0.03
#case_predict$seventy_hosp_pred = case_predict$seventy   *0.08
#case_predict$old_hosp_pred     = case_predict$old       *0.15

#case_predict$young_hosp_pred   = case_predict$young     *0.002
#case_predict$twenty_hosp_pred  = case_predict$twenty    *0.003
#case_predict$thirty_hosp_pred  = case_predict$thirty    *0.006
#case_predict$forthy_hosp_pred  = case_predict$forthy    *0.0075
#case_predict$fithy_hosp_pred   = case_predict$fithy     *0.013
#case_predict$sixty_hosp_pred   = case_predict$sixty     *0.025
#case_predict$seventy_hosp_pred = case_predict$seventy   *0.06
#case_predict$old_hosp_pred     = case_predict$old       *0.127


case_predict$young_hosp_pred   = case_predict$young     *0.003
case_predict$twenty_hosp_pred  = case_predict$twenty    *0.003 #5
case_predict$thirty_hosp_pred  = case_predict$thirty    *0.006
case_predict$forthy_hosp_pred  = case_predict$forthy    *0.0075
case_predict$fithy_hosp_pred   = case_predict$fithy     *0.013
case_predict$sixty_hosp_pred   = case_predict$sixty     *0.03
case_predict$seventy_hosp_pred = case_predict$seventy   *0.07
case_predict$old_hosp_pred     = case_predict$old       *0.14


case_predict$hosp_adm_rate = case_predict$young_hosp_pred + case_predict$twenty_hosp_pred+
                              case_predict$thirty_hosp_pred + case_predict$forthy_hosp_pred+
                              case_predict$fithy_hosp_pred +case_predict$sixty_hosp_pred +
                              case_predict$seventy_hosp_pred + case_predict$old_hosp_pred

colnames(case_predict) <- c("young", "twenty", "thirty", "forthy",
                            "fithy", "sixty", "seventy", "old",
                            "date",
                            "0-19", "20-29", "30-39", "40-49", "50-59", 
                            "60-69", "70-79", "80+","hosp_adm_rate")

case_predict$date = case_predict$date+7

keycol <- "date"
valuecol <- "type"
gathercols <- c("0-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
case_predict_input <- gather(case_predict, keycol, valuecol, gathercols)





hosp_real.file  <- paste0("https://raw.githubusercontent.com/mzelst/covid-19/master/data-nice/age/leeftijdsverdeling_datum_Klinisch_IC_long.csv")
hosp_real <-read.csv(hosp_real.file ,sep=",", check.names = FALSE)
hosp_real$Datum <- as.Date(hosp_real$Datum)

#### select type

type.of.data = "Klinisch"


hosp_real <- (hosp_real  %>% filter(Type == type.of.data & Datum > "2021-01-01")) # & Datum < "2021-02-01"))  #  Klinisch


#### make agegroups ####

hosp_real$young  =hosp_real$`<20`
hosp_real$twenty = hosp_real$`20 - 24` + hosp_real$`25 - 29`
hosp_real$thirty = hosp_real$`30 - 34` + hosp_real$`35 - 39`
hosp_real$four   = hosp_real$`40 - 44` + hosp_real$`45 - 49`
hosp_real$five   = hosp_real$`50 - 54` + hosp_real$`55 - 59`
hosp_real$six    = hosp_real$`60 - 64` + hosp_real$`65 - 69`
hosp_real$seven  = hosp_real$`70 - 74` + hosp_real$`75 - 79`
hosp_real$old    = hosp_real$`80 - 84` + hosp_real$`85 - 89` +  hosp_real$`>90`


#### calc increase per day ####

hosp_real <- (hosp_real %>%  mutate(young.Change  = young  - lag(young,  default = young[1])) )
hosp_real <- (hosp_real %>%  mutate(twenty.Change = twenty - lag(twenty, default = twenty[1])) )
hosp_real <- (hosp_real %>%  mutate(thirty.Change = thirty - lag(thirty, default = thirty[1])) )
hosp_real <- (hosp_real %>%  mutate(four.Change   = four   - lag(four,   default = four[1])) )
hosp_real <- (hosp_real %>%  mutate(five.Change   = five   - lag(five,   default = five[1])) )
hosp_real <- (hosp_real %>%  mutate(six.Change    = six    - lag(six,    default = six[1])) )
hosp_real <- (hosp_real %>%  mutate(seven.Change  = seven  - lag(seven,  default = seven[1])) )
hosp_real <- (hosp_real %>%  mutate(old.Change    = old    - lag(old,    default = old[1])) )


hosp_real <- hosp_real[,-c(2:26)]


#hosp_real.file <-paste("C:\\Rdir\\data\\2021-11-04\\2021-11-04_nice_instroom_hosp.csv")
#hosp_real <- read.csv(hosp_real.file,sep=";")
colnames(hosp_real) <- c("date","0-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
hosp_real$date = as.Date(hosp_real$date)


hosp_real$`0-19`  = rollmeanr(hosp_real$`0-19`  , 7, fill = 0)
hosp_real$`20-29`  = rollmeanr(hosp_real$`20-29` , 7, fill = 0)
hosp_real$`30-39`  = rollmeanr(hosp_real$`30-39`, 7, fill = 0)
hosp_real$`40-49` = rollmeanr(hosp_real$`40-49` , 7, fill = 0)
hosp_real$`50-59`   = rollmeanr(hosp_real$`50-59` , 7, fill = 0)
hosp_real$`60-69`   = rollmeanr(hosp_real$`60-69` , 7, fill = 0)
hosp_real$`70-79` = rollmeanr(hosp_real$`70-79` , 7, fill = 0)
hosp_real$`80+`  = rollmeanr(hosp_real$`80+` , 7, fill = 0)


keycol <- "date"
valuecol <- "type"
gathercols <- c("0-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
hosp_real_testdata <- gather(hosp_real, keycol, valuecol, gathercols)


ggplot()+
  geom_point(data=hosp_real_testdata, aes(x=date, y=valuecol, color=keycol), size=2)+
  geom_line(data=case_predict_input, aes(x=date, y=valuecol, color=keycol))+
  
  facet_wrap(~ keycol,  scales = "free_y")+
  #facet_wrap(~ keycol)+

  scale_x_date(date_breaks = "1 month", 
             date_labels= format("%b"),
             limits = as.Date(c("2021-08-01", NA)))+
  labs(title = "Bierviltje - ziekenhuisopnames",
       subtitle = "Dit klopt niet, want bierviltje.",
       caption = paste("Source: NICE / RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme( plot.title = element_text(hjust = 0.5,size = 30,face = "bold"))

ggsave("data/plots/bierviltje_plus_2.png",width=16, height = 9)



# Intake per day of patients in hospital (non-IC) with suspected and/or proven covid-19
json_zkh_df <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/new-intake/",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill=TRUE)

zkh_new <- as.data.frame(t(json_zkh_df[c(1,2,4),]))

zkh_new$date <- unlist(zkh_new$V1)
zkh_new$new_hosp_proven <- unlist(zkh_new$V2)
zkh_new$new_hosp_suspected <- unlist(zkh_new$V3)
zkh_new <- zkh_new[,c(4:6)]

zkh_new$sum = zkh_new$new_hosp_proven+zkh_new$new_hosp_suspected

zkh_new$date = as.Date(zkh_new$date)

zkh_new <- zkh_new[zkh_new$date>"2021-03-20",]


ggplot()+
  geom_line(data=case_predict, aes(x=date, y=hosp_adm_rate), color = "blue", size = 2)+
  
  geom_line(data=zkh_new, aes(x=date, y=sum), color = "black", size=1)+
  
  geom_point(data=zkh_new, aes(x=date, y=sum), color = "gray", size=3)+
  geom_point(data=zkh_new, aes(x=date, y=sum), color = "black", size=2)+
  
  
  scale_y_continuous(limits = c(0,NA))+
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%b"),
               limits = as.Date(c("2021-04-01", NA)))+
  labs(title = "Bierviltje - ziekenhuisopnames",
       subtitle = "Dit klopt niet, want bierviltje.\nLaatste 3 datapunten nog niet gecorrigeerd voor rapportagevertraging",
       caption = paste("Source: NICE / RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme( plot.title = element_text(hjust = 0.5,size = 30,face = "bold"))

ggsave("data/plots/bierviltje_plus_hosp.png",width=16, height = 9)



