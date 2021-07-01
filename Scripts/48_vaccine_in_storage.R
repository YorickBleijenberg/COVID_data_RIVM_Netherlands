
today = Sys.Date()
yesterday = today-1

# vacc_date_hist.file <- paste0("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/vaccination/daily-dashboard-update/",today,"_vaccine-data.csv")
# geprikt <-read.csv(vacc_date_hist.file,sep=",")

geprikt <- new.vacc.df

aantal.doses.gezet <- last(geprikt$estimated_new)

opdeplank <-read.csv("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/vaccination/people.vaccinated%20-%20doses.received.csv",sep=",")

opdeplank[is.na(opdeplank)] <- 0

geprikt <- geprikt[ -c(1,3,4,6:18)]
opdeplank <- opdeplank[ -c(2:10,12:33)]

colnames(geprikt) <- c("date", "dose_given")
colnames(opdeplank) <- c("date","dose_recieved")
  
opdeplank$date <- as.Date(opdeplank$date)

opdeplank <- merge(opdeplank, geprikt, by ="date", all =  TRUE )

#
#

opdeplank <- filter(opdeplank, date > "2020-12-24")
opdeplank <- filter(opdeplank, date < today)


opdeplank[is.na(opdeplank)] <- 0
opdeplank$time.a=0


opdeplank$date <- as.Date(opdeplank$date)

#### calculate number since the dose was received

n=1
d=1

while (n < (nrow(opdeplank)+1)){
  given.value <- opdeplank$dose_given[n]
  x=1
  while (x < n){
    if (opdeplank$dose_recieved[x] <= opdeplank$dose_given[n]){
      opdeplank$time.a[n] = n-x
    } else{
      opdeplank$time.a[n] = n-x
        break
      }
    x=x+1  
   }
  n <- n+1
}

#### subtitle

days.in.storage <- last(opdeplank$time.a)
aantal.doses.gezet <- format(aantal.doses.gezet ,big.mark = ".", decimal.mark = ",")
storage.subtitle <- paste0("De ", aantal.doses.gezet, 
                           " doses die gisteren zijn weggeprikt, zijn ", days.in.storage, " dagen geleden binnengekomen.")

colnames(opdeplank) <- c("date", "dose_recieved", "dose_given", "Dagen")

#### plot  ####
ggplot(opdeplank, aes(x=date, y=Dagen))+
  
  geom_line(mapping = aes(y=21), color = "darkred",lwd = 2)+
  geom_line(mapping = aes(y=14), color = "darkred",lwd = 2)+
  geom_line(mapping = aes(y=7), color = "darkred",lwd = 2)+
  
  annotate("text", x = as.Date("2021-01-06"), y = 22, label = "3 weken", size=10,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2021-01-06"), y = 15, label = "2 weken", size=10,color = "black",face = "bold", hjust ="left")+
  annotate("text", x = as.Date("2021-01-06"), y = 8, label = "1 week", size=10,color = "black",face = "bold", hjust ="left")+
  
  geom_point(size=3)+
  geom_line(size =2)+
  
  theme_classic()+
  
  xlab("")+
  #ylab("")+
  
  scale_y_continuous(limits = c(0, 30))+
  
  scale_x_date(date_breaks  = "1 month",
               date_labels = "%b",
               limits  = as.Date(c("2021-01-06", as.character(yesterday) ))
               )+
  
  labs(title     = "Aantal dagen dat de geprikte dosis in de vriezer lag",
       subtitle = storage.subtitle,
       caption   = paste("Bron: VWS | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(
    plot.background    = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background   = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title         = element_text(hjust = 0.5 ,size = 40 ,face = "bold"),
    plot.subtitle      = element_text(hjust=0.5   ,size = 25 ,color = "black", face = "italic"),
    axis.text          = element_text(size=14,color = "black",face = "bold"),
    axis.ticks         = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length  = unit(0.5, "cm"),
    axis.line          = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+

ggsave("data/plots/80_vaccine_on_shelf.png",width=16, height = 9)

