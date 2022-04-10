
#require(rjson)
require(data.table)
require(jsonlite)


## Parse daily percentage positive tests - national ##



tested_daily <- fromJSON(txt = "https://data.rivm.nl/covid-19/COVID-19_uitgevoerde_testen.json")

tested_daily$Date_of_statistics <- as.Date(tested_daily$Date_of_statistics)

#tested_daily.count <- count(tested_daily,Date_of_statistics, Tested_with_result, Tested_positive)

tests.wide          <- aggregate(Tested_with_result ~ Date_of_statistics, data = tested_daily, FUN = sum)
tests.positive.wide <- aggregate(Tested_positive ~ Date_of_statistics, data = tested_daily, FUN = sum)
tested_daily.count <- merge(tests.wide,tests.positive.wide, by = c("Date_of_statistics"))

tested_daily.count <- tested_daily.count %>%
  mutate(pos.rate = Tested_positive/Tested_with_result*100) %>%
  mutate(tests_7davg = round(frollmean(Tested_with_result,7),0))

colnames(tested_daily.count) <- c("date", "values.tested_total", "positive_tests", "values.infected_percentage", "MA_perc")

tested_daily.count$fact <- (1000 * tested_daily.count$values.infected_percentage)
tested_daily.count$MA_perc  <- round(frollmean(tested_daily.count$values.infected_percentage,7),1)

tested_daily.count$MA_perc_lead  <- lead(tested_daily.count$MA_perc,3)

tested_daily.count$MA_perc_fact <- (1000 * tested_daily.count$MA_perc_lead)


tested_daily.count$Ma_tot_tests <-  round(frollmean(tested_daily.count$values.tested_total ,7),0)
tested_daily.count$Ma_tot_tests_lead  <- lead(tested_daily.count$Ma_tot_tests,3)

file.ggd.today <- paste0("data-dashboards/percentage-positive/", Sys.Date() ,"_percentage-positive-daily-national.csv")

write.csv(tested_daily.count, file = file.ggd.today)




tested_daily.count$values.infected_percentage  <- round(tested_daily.count$values.infected_percentage,1)

date.last.value   <- last(tested_daily.count$date)
last.pos.value    <- last(tested_daily.count$values.infected_percentage)
last.pos_ma.value <- last(tested_daily.count$MA_perc)
last.tests.value  <- format(last(tested_daily.count$values.tested_total), big.mark="." ,decimal.mark=",")
last.tests.pos.value  <- format(last(tested_daily.count$positive_tests), big.mark="." ,decimal.mark=",")



values.subtitle <- paste0("Datum laatste datapunt: ",date.last.value, "\n aantal testen: ",last.tests.value,"\n",
                          "Aantal testen positief: ", last.tests.pos.value, 
                          "\n Percentage positief: ", last.pos.value,  "%    ---   7-daags gemiddelde percentage: ",last.pos_ma.value, "%")



tested_daily.count$values.tested.min.pos  <- tested_daily.count$values.tested_total-tested_daily.count$positive_tests




ggplot(data = tested_daily.count,)+  
  
 # geom_bar(stat='identity', mapping = aes(x = date, y = values.tested_total), fill = "#ED7D31")+
  geom_bar(stat='identity', mapping = aes(x = date, y = positive_tests), fill = "red", color="red")+
  
  
 # geom_line(mapping = aes(x = date, y = Ma_tot_tests_lead), colour = "#F5F5F5", size = 2 )+
  #geom_line(mapping = aes(x = date, y = Ma_tot_tests_lead), colour = "#9e480e", size = 1 )+
  
 # geom_line(mapping = aes(x = date, y = fact), colour = "#FFFFFF", size = 0.5 )+
 # geom_line(mapping = aes(x = date, y = fact), colour = "#4472C4", size = 1 )+
  
  geom_point(mapping = aes(x = date, y = fact), colour = "black",size = 2) +
  geom_point(mapping = aes(x = date, y = fact), colour = "#F5F5F5",size = 1,alpha = 0.8) +
  
    geom_line(mapping = aes(x = date, y = MA_perc_fact), colour = "black", size = 3 )+
  
  
  
  
  
  scale_y_continuous(limits = c(0, 70000)   ,labels = label_number(big.mark = ".", decimal.mark = ","), 
                     sec.axis = sec_axis(~ . / 100000, labels = percent))+
  
  
  scale_x_date(date_breaks = "1 month", 
               
            
               
               date_labels= format("%b"),
               limits = as.Date(c("2021-01-01", NA)))+
  
 # geom_vline(xintercept = as.numeric(tested_daily$date [dates_vline_mondays]),
#                       col = "gray", lwd = 0.2, linetype= "dashed")+
  
  coord_cartesian(expand = TRUE)+
  
  theme_classic()+
  
  labs(x = "",
       y = "",
       title = " Aantal positive testen & percentage positief bij de GGD'en",
      subtitle = values.subtitle,
       caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme( plot.background = element_rect(fill = "#F5F5F5"),
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         legend.position = "none",   # no legend
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         axis.text = element_text(size=25,color = "black",face = "bold"),
         axis.text.y = element_text(size=25, face="bold", color="black"),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
         axis.line = element_line(colour = "#F5F5F5"),
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
         
         axis.text.y.left = element_text(color = "red"),
         axis.text.y.right = element_text(color = "black")
  )+
  
ggsave("data/plots/22_tests_ggd_daily_Beau.png",width=16, height = 9)


