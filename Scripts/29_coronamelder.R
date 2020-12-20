


#Update GGD confirmed positive tests statistics


###  Column name	Description	Format
# - Date	Date for this entry	YYYY-MM-DD
# - Reported positive tests through app authorised by GGD (daily)	Number of times a GGD key was authorised for the specified date	Number
# - Reported positive test through app authorised by GGD (cumulative)	Cumulative number of positive tests that are authorised by the GGD up to and including the specified date	Number



Coronamelder.path <- paste0("https://github.com/minvws/nl-covid19-notification-app-statistics/raw/main/statistics/ggd_positive_test_authorisations.csv")
c19.app <- read.csv(Coronamelder.path,sep=",") 

colnames(c19.app) = c("date", "positive_daily","positive_cumulative")

c19.app$date <- as.Date(c19.app$date)




ggplot(c19.app)+
  geom_col(aes(date, positive_daily), fill = "orange")+
  
  theme_classic() + 
  xlab("")+ 
  ylab("")+
  
  scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  labs(title = "Positieve testen in de CoronaMelder App",    #GGD positive test authorizations
       subtitle = "Aantal keer dat de GGD de unieke code van de gebruiker authoriseert om ook via de App, contacten een melding te sturen",
       caption = paste("Bron: minvws | Plot: @YorickB | ",Sys.Date()))+
  
  theme( plot.background = element_rect(fill = "#F5F5F5"),
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         axis.text = element_text(size=20,color = "black",face = "bold"),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
         axis.line = element_line(colour = "#F5F5F5"),
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
         legend.position = "none"
  )
  
  
  ggsave("data/81_coronamelder.png",width=16, height = 9)

  
  
  
  Merged_data_short$date <- Merged_data_short$fixedDate
  
  app.percentage <-  merge(Merged_data_short, c19.app)
  app.percentage$cases_min_not  <-  (app.percentage$cases  - app.percentage$positive_daily)
  app.percentage <- app.percentage[-c(2:16,18)]
  
  app.percentage_gather <- gather(app.percentage, cases_min_not, positive_daily, -date)
  
  colnames(app.percentage_gather) <- c("date", "type","value")
  app.percentage_gather$type <- as.factor(app.percentage_gather$type)
  
  
  
  ggplot(app.percentage_gather, aes(date, value, fill=type))+
    geom_col()+
    
   # scale_fill_manual(values=c("#96afde","#44546a"),labels=c("Aantal testen zonder melding","Aantal testen met melding via de app"))+
    
    theme_classic() + 
    xlab("")+ 
    ylab("")+
    
    scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","))+
    
    labs(title = "Positieve testen in de CoronaMelder App",    #GGD positive test authorisations
         subtitle = "Aantal keer dat de GGD de unieke code van de gebruiker authoriseert om ook via de App, contacten een melding te sturen",
         caption = paste("Bron: minvws | Plot: @YorickB | ",Sys.Date()))+
    
    theme(legend.position = "top",
          legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
          legend.title = element_blank(),
          legend.text = element_text(colour="black", size=10, face="bold"),
          )+
    
    theme( plot.background = element_rect(fill = "#F5F5F5"),
           panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
           plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
           plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
           axis.text = element_text(size=20,color = "black",face = "bold"),
           axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
           axis.ticks.length = unit(0.5, "cm"),
           axis.line = element_line(colour = "#F5F5F5"),
           panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
    )
  
  
  ggsave("data/82_coronamelder-total.png",width=16, height = 9)
  
  