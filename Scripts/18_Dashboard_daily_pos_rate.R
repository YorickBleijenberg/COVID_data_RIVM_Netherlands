
#require(rjson)
require(data.table)
require(jsonlite)


## Parse daily percentage positive tests - national ##

dat <- fromJSON(txt = "https://coronadashboard.rijksoverheid.nl/json/NL.json")
tested_daily <- as.data.frame(dat$tested_ggd_daily[1])
tested_daily$date <- as.Date(as.POSIXct(tested_daily$values.date_unix, origin="1970-01-01"))
tested_daily$fact <- (4000 * tested_daily$values.infected_percentage)

write.csv(tested_daily, file = "data-dashboards/percentage-positive/percentage-positive-daily-national.csv")


dates_vline_mondays <- as.Date(c("2020-08-10","2020-08-17","2020-08-24","2020-08-31","2020-09-07",
                                 "2020-09-14","2020-09-21","2020-09-28","2020-10-05","2020-10-12",
                                 "2020-10-19","2020-10-26","2020-11-02","2020-11-09","2020-11-16",
                                 "2020-11-23","2020-11-30","2020-12-07","2020-12-14","2020-12-21",
                                 "2020-12-28","2021-01-04","2021-01-11", "2021-01-18","2021-01-25")) 

dates_vline_mondays <- which((tested_daily$date %in% dates_vline_mondays))


date.last.value <- last(tested_daily$date)
last.pos.value <- last(tested_daily$values.infected_percentage)
last.tests.value <- format(last(tested_daily$values.tested_total), big.mark="." ,decimal.mark=",")


values.subtitle <- paste0("Datum laatste datapunt: ",date.last.value, "    ---    Percentage: ",last.pos.value,"%    ---    aantal testen: ", last.tests.value)


ggplot(data = tested_daily,)+  
  geom_bar(stat='identity', mapping = aes(x = date, y = values.tested_total), fill = "#ED7D31")+
  geom_line(mapping = aes(x = date, y = fact), colour = "#FFFFFF", size = 0.5 )+
  geom_line(mapping = aes(x = date, y = fact), colour = "#4472C4", size = 1 )+
  
  geom_point(mapping = aes(x = date, y = fact), colour = "#FFFFFF",size = 1) +
  geom_point(mapping = aes(x = date, y = fact), colour = "#4472C4",size = 2,alpha = 0.8) +
  
  scale_y_continuous(limits = c(0, 89000), labels = label_number(big.mark = ".", decimal.mark = ","),
                     sec.axis = sec_axis(~ . / 4000))+
  
  scale_x_date(date_breaks = "2 week", 
               date_labels= format("%d/%m"),
               limits = as.Date(c("2020-06-03", NA)))+
  
 geom_vline(xintercept = as.numeric(tested_daily$date [dates_vline_mondays]),

                       col = "gray", lwd = 0.2, linetype= "dashed")+
  
  coord_cartesian(expand = FALSE)+
  
  theme_classic()+
  
  labs(x = "",
       y = "",
       title = " Aantal testen & percentage positief GGD'en",
       subtitle = values.subtitle,
       caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme( plot.background = element_rect(fill = "#F5F5F5"),
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         legend.position = "none",   # no legend
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=14),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
         axis.line = element_line(colour = "#F5F5F5"),
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
         
         axis.text.y.left = element_text(color = "#ED7D31"),
         axis.text.y.right = element_text(color = "#4472C4")
  )+
  
ggsave("data/22_tests_ggd_daily.png",width=16, height = 9)
