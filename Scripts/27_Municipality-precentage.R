
### cumultative numbers

#### import munacipality data ####

number_tot_cumu <- RIVM_aantallen_gemeente_cumulatief


#### calculate cumulative #####

number_tot_cumu_sh <- number_tot_cumu[,c("Date_of_report","Municipality_name", "Total_reported")]
number_tot_cumu_sh$Date_of_report <- as.Date(number_tot_cumu_sh$Date_of_report)


inwo_gem <- "C:\\Rdir\\data-contstant\\CBS_inwoners_gemeente.csv"
gemeente.inwoners <- read.csv(inwo_gem,sep=";")  
colnames(gemeente.inwoners) = c("Municipality_code", "Municipality_name", "inwoners", "gemeente_getal")


number_tot_cumu_sh.2 <- merge(number_tot_cumu_sh,gemeente.inwoners)

number_tot_cumu_sh.2$perc<- (number_tot_cumu_sh.2$Total_reported/number_tot_cumu_sh.2$inwoners)

ggplot(data = number_tot_cumu_sh.2) + 
  geom_line(stat='identity', mapping = aes(x = Date_of_report, y = perc, color = Municipality_name))+
  theme( legend.position = "none")+   # no legend
  scale_y_continuous(limits = c(0, NA),  label = percent_format())

ggsave("data/25_perc_cases_city.png",width=16, height = 9)




File_date_muni_cumi <- paste0("data/city_cumulative.csv")
write.csv2(number_tot_cumu_sh.2, File_date_muni_cumi, row.names=FALSE)
