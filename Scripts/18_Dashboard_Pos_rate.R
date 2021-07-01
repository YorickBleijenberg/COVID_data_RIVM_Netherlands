##  https://coronadashboard.rijksoverheid.nl/_next/data/w6-OHcQdWWXMQljvujpzJ/landelijk/positief-geteste-mensen.json

library(jsonlite)
#dasdb<-fromJSON(txt = "https://coronadashboard.rijksoverheid.nl/json/VR_COLLECTION.json")
dasdb<-fromJSON(txt = "https://coronadashboard.rijksoverheid.nl/_next/data/w6-OHcQdWWXMQljvujpzJ/landelijk/positief-geteste-mensen.json")
db<-dasdb[["pageProps"]][["data"]][["tested_ggd_average"]][["values"]]
as.POSIXct(db$date_end_unix,origin="1970-01-01 00:00:00")



## Parse daily percentage positive tests - safety region ##

df.vr.dailytests <- data.frame()

for (i in 1:25) {
  if(i<10){
    db <- fromJSON(txt=paste0("https://coronadashboard.rijksoverheid.nl/json/VR0",i,".json"))
  }else{
    db <- fromJSON(txt=paste0("https://coronadashboard.rijksoverheid.nl/json/VR",i,".json"))
  }
  db <- as.data.frame(db$tested_ggd_daily[1])
  df.vr.dailytests <- rbind(df.vr.dailytests,db)
}
df.vr.dailytests$date <- as.Date(as.POSIXct(df.vr.dailytests$values.date_unix, origin="1970-01-01"))

write.csv(df.vr.dailytests, file = "data-dashboards/percentage-positive/percentage-positive-daily-safetyregion.csv")






db$date <- as.Date(as.POSIXct(db$date_end_unix, origin="1970-01-01"))

db <- db[,-c(1,2,6)]

today <- Sys.Date()

Pos.rate.File <- paste0("data-dashboards/positive-rate/positive-rate",today,".csv")
write.csv(db, file =Pos.rate.File , row.names = F)





dates_vline_mondays <- as.Date(c("2020-08-10","2020-08-17","2020-08-24",
                                 "2020-08-31",
                                 "2020-09-07",
                                 "2020-09-14",
                                 "2020-09-21",
                                 "2020-09-28",
                                 "2020-10-05",
                                 "2020-10-12",
                                 "2020-10-19",
                                 "2020-10-26",
                                 "2020-11-02",
                                 "2020-11-09",
                                 "2020-11-16",
                                 "2020-11-23",
                                 "2020-11-30",
                                 "2020-12-07",
                                 "2020-12-14",
                                 "2020-12-21",
                                 "2020-12-28",
                                 "2021-01-04"
))   

dates_vline_mondays <- which((df4$Datum %in% dates_vline_mondays))

db <- tested_daily

db$fact <- (4000 * db$infected_percentage)



ggplot(data = db,)+  
  geom_bar(stat='identity', mapping = aes(x = date, y = tested_total), fill = "#ED7D31")+
  geom_line(mapping = aes(x = date, y = fact), colour = "#FFFFFF", size = 1 )+
  geom_line(mapping = aes(x = date, y = fact), colour = "#4472C4", size = 2 )+
  
  geom_point(mapping = aes(x = date, y = fact), colour = "#FFFFFF",size = 2) +
  geom_point(mapping = aes(x = date, y = fact), colour = "#4472C4",size = 3,alpha = 0.8) +
  
  
 scale_y_continuous(limits = c(0, 75000), labels = label_number(big.mark = ".", decimal.mark = ","),
                    sec.axis = sec_axis(~ . / 4000))+
  
 
  scale_x_date(date_breaks = "1 month", 
               date_labels= format("%m"),
               limits = as.Date(c("2020-10-01", NA)))+
  
  geom_vline(xintercept = as.numeric(df4$Datum[dates_vline_mondays]),
             col = "darkgray", lwd = 1, linetype= "dashed")+
  
  
  coord_cartesian(expand = FALSE)+
  
  
  theme_classic()+
  
  labs(x = "",
       y = "",
       title = " Aantal testen & percentage positief GGD'en", #text_title,
       #subtitle = paste("tabel 13."),
       caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         legend.position = "none",   # no legend
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
         axis.line = element_line(colour = "#F5F5F5"),
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
         
         axis.text.y.left = element_text(color = "#ED7D31"),
         axis.text.y.right = element_text(color = "#4472C4")
  )+
  
 # geom_text( aes( x=42.5, y=550000, label=text_sub),
  #           color="#ED7D31", 
     #        size=7 , angle=0, fontface="bold")+
  
  
 # annotate("curve", x = 49.5, xend =52, 
     #      y = 550000, yend = 500000, curvature = -0.2,
      #     colour = "black", size=2, alpha=0.7, arrow =arrow(type = "open",length = unit(2,"mm")))


ggsave("data/22_tests_ggd_daily.png",width=16, height = 9)


