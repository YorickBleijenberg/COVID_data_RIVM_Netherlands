library(jsonlite)
library(tidyverse)
library(gganimate)
library(lubridate)
library(zoo)




vr <- "C:\\Rdir\\data-contstant\\veiligheidsregios.csv"
VR <- read.csv(vr,sep=";")  
colnames(VR) = c("vrcode", "Regio_Naam", "inwoners")

VR$inwoners <- as.integer(VR$inwoners)

FR_a <- intToUtf8(0xC2)  #Encoding(FR_b) <- "UTF-8"
FR_b <- paste0("Frysl", FR_a,"n")
VR$Regio_Naam <- str_replace(VR$Regio_Naam, "Fryslân", FR_b)  ##fout / goed



#### code bij Jan Borst ####
tot<-data.frame()
i=1

#### https://coronadashboard.rijksoverheid.nl/_next/data/Zu6pE8P0LzYqDDvmsb8WQ/veiligheidsregio/VR22/positief-geteste-mensen.json
####02/positief-geteste-mensen.json


db<-fromJSON(txt=paste0("https://coronadashboard.rijksoverheid.nl/_next/data/Zu6pE8P0LzYqDDvmsb8WQ/veiligheidsregio/VR22/positief-geteste-mensen.json"))
db<-db[["pageProps"]][["data"]][["ggd"]][["values"]]


for (i in 1:25) {
  if(i<10){
    db<-fromJSON(txt=paste0("https://coronadashboard.rijksoverheid.nl/_next/data/TtTqx1URGaEt7QGeaxKZA/veiligheidsregio/VR0",i,"/positief-geteste-mensen.json"))
  }else{
    db<-fromJSON(txt=paste0("https://coronadashboard.rijksoverheid.nl/_next/data/TtTqx1URGaEt7QGeaxKZA/veiligheidsregio/VR",i,"/positief-geteste-mensen.json"))
  }
  db<-db[["pageProps"]][["data"]][["ggd"]][["values"]]
  tot<-rbind(tot,db)
}

#### Merge df's and clean up ####

VR_poss_rate_values <- merge(tot,VR)

VR_poss_rate_values$date <- as.Date(as.POSIXct(VR_poss_rate_values$week_unix, origin="1970-01-01"))

File_date_VR <- paste0("rivm-dashboard/VR/VR_", format(Sys.time(), "%Y-%m-%d"),".csv")
write.csv2(VR_poss_rate_values, File_date_VR, row.names=FALSE)

#### plot positivity regions ####

ggplot(VR_poss_rate_values, aes(x=date, y=infected_percentage, fill = Regio_Naam))+
 
  
  annotate("rect", xmin = as.Date("2020-06-01"), xmax = as.Date("2020-11-10"), ymin = 0, ymax =4, color = "black", fill = "lightgreen", alpha = 0.3)+
  annotate("rect", xmin = as.Date("2020-06-01"), xmax = as.Date("2020-11-10"), ymin = 4, ymax =5, color = "black", fill = "orange", alpha = 0.3)+
  
  #geom_line(mapping = aes(x=date, y=5), color = "darkgreen",lwd = 1, linetype = "twodash")+
  #geom_line(mapping = aes(x=date, y=3), color = "black",lwd = 1,linetype = "dotted")+
  
   geom_line(colour = "darkred", size =1.5)+
  facet_wrap(~Regio_Naam,)+
  
  theme_bw() + 
  xlab("")+ 
  ylab("")+
  
  
  labs(title = "Percentage Tests Positief",
       x = "",
       y = "", #Percentage positief",
      # color = "Legend"
      subtitle = "orange - WHO grens: 5% \n groen - ECDC grens:  4%",
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         legend.position = "none",   # no legend
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=12),  #, angle=45),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
      
            # axis.line = element_line(colour = "#F5F5F5"),
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
        
          ### facet label custom
         strip.text.x = element_text(size = 13, color = "black"),
         strip.background = element_rect(color="black", fill="gray", size=1.5, linetype="solid"),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
  )
  
  
  ggsave("data/50_percentage_positief_VR.png",width=15, height = 15)
  
  
  
  
  


  
  
  
  
  