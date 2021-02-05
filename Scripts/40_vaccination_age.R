





people.vaccinated.gh <-read.csv("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/vaccination/people.vaccinated.csv",sep=",")
people.vaccinated.gh$date <- as.Date(people.vaccinated.gh$date)

agecbs.df <-read.csv("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/data-cbs/people.vaccinated%20-%20age-reverse.csv",sep=",")




ggplot(people.vaccinated.gh)+
geom_col(aes(x= date, y=total_vaccinations))+
  
  geom_hline(data=agecbs.df, mapping=aes(yintercept=aantal), color="black")+
 geom_text(data=agecbs.df, mapping=aes(x=as.Date("2021-01-01"), y=aantal, label=Leeftijd), size=4, vjust=-0.4, hjust=0)+

scale_y_continuous(limits = c(0, 18000000),breaks = c(5000000,10000000,15000000), labels = label_comma(big.mark = ".", decimal.mark = ","))+

ggsave("data/92_vaccine_age_full.png",width=5, height = 25)

 
ggplot(people.vaccinated.gh)+
  geom_col(aes(x= date, y=total_vaccinations))+
  
  geom_hline(data=agecbs.df, mapping=aes(yintercept=aantal), color="black")+
  geom_text(data=agecbs.df, mapping=aes(x=as.Date("2021-01-01"), y=aantal, label=Leeftijd), size=4, vjust=-0.4, hjust=0)+
  
  scale_y_continuous(limits = c(0, 500000),breaks = c(5000000,10000000,15000000), labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  ggsave("data/92_vaccine_age_small.png",width=16, height = 9)

