library(tidyverse)

today <- Sys.Date()

inwo_gem <- "C:\\Rdir\\data-contstant\\CBS_inwoners_gemeente.csv"
gemeente.inwoners <- read.csv(inwo_gem,sep=";")  
colnames(gemeente.inwoners) = c("Municipality_code", "Gemeente_Naam", "inwoners", "gemeente_getal")



FR_a <- intToUtf8(0x00E2)  #Encoding(FR_b) <- "UTF-8"
FR_u <- intToUtf8(0x00FA)  #Encoding(FR_b) <- "UTF-8"
SFR_name <- paste0("S", FR_u, "dwest Frysl", FR_a,"n")
SFR_name2 <- paste0("Noardeast-Frysl", FR_a,"n")

gemeente.inwoners$Gemeente_Naam <- str_replace(gemeente.inwoners$Gemeente_Naam, "Súdwest Fryslân", SFR_name)  ##fout / goed

gemeente.inwoners$Gemeente_Naam <- str_replace(gemeente.inwoners$Gemeente_Naam, "Noardeast-Fryslân", SFR_name2)  ##fout / goed





read.aantal.gemeente.path <- paste("C:\\Rdir\\data\\",Sys.Date(),"\\", Sys.Date(), "_COVID-19_aantallen_gemeente_per_dag.csv",sep="")
RIVM_aantallen_gemeente_per_dag <- read.csv(read.aantal.gemeente.path,sep=";")



RIVM_aantallen_gemeente_per_dag$date <- as.Date(RIVM_aantallen_gemeente_per_dag$date)
RIVM_aantallen_gemeente_per_dag.1  <- RIVM_aantallen_gemeente_per_dag[ -c(1,2,4:9,11,12,14)]

RIVM_aantallen_gemeente_per_dag.combi <- merge(RIVM_aantallen_gemeente_per_dag.1,gemeente.inwoners)

RIVM_aantallen_gemeente_per_dag.combi$phd <- (RIVM_aantallen_gemeente_per_dag.combi$Total_reported/RIVM_aantallen_gemeente_per_dag.combi$inwoners)*100000

combi.1 <- aggregate(RIVM_aantallen_gemeente_per_dag.combi$phd,     by=list(date=RIVM_aantallen_gemeente_per_dag.combi$date,
                                                                                                            gemeente_Naam=RIVM_aantallen_gemeente_per_dag.combi$Gemeente_Naam,
                                                                                                            gemeente_getal=RIVM_aantallen_gemeente_per_dag.combi$gemeente_getal), 
                                               FUN=sum)
colnames(combi.1) = c("date", "gemeente_Naam", "gemeente_getal","phd")

combi.1 <- subset(combi.1, gemeente_Naam !='')


#combi.1 <- combi.1[combi.1$date>"2020-09-01",]
combi.1 <- combi.1[combi.1$date>"2020-01-05",]    


  #### Calculate the 7 day MA per gemeente per day ####


combi.2 <- combi.1 %>% 
             group_by(gemeente_Naam) %>% 
                mutate(MAphd = rollapply(phd, 7, mean, fill = NA, align = "right"))


# RIVM_aantallen_gemeente_per_dag.combi.2 <- subset(RIVM_aantallen_gemeente_per_dag.combi.2, Municipality_name == 'Urk')




kleur.table  = data.frame()

i=1
while (i < 353)
{
  
  combi.iteration <- combi.2    #copy the merged municipality data
  combi.iteration <- combi.iteration[combi.iteration$gemeente_getal == i,]  #select only one municipality to evaluate.
  
  v_today <- last(combi.iteration$MAphd)                       ### get the current value (per 100K).  MAphd = moving average per 100K
  bb <- last(combi.iteration$MAphd,15)
  v_14d <- head(tail(combi.iteration$MAphd, n=15), n=1)       ### get the value (per 100K) of 14 days ago
  v_7d <- head(tail(combi.iteration$MAphd, n=8), n=1)        ### get the value (per 100K) of 7 days ago
  
  ### assign color value to municipality, according to the current trend.
  kleur <- paste("yellow")                              ### default value = yellow
  if (((v_today > v_14d+2) | (v_today > v_7d+3))&(v_today < v_7d+7)) {       ### red if the current value is high than 14d ago+2 OR higher than 7 days ago +3
    kleur <- paste("red")
  } else if (v_today > v_7d+7) {                              ### darkred if the the difference is more than +13
    kleur <- paste("help")
  } else if (((v_today < v_14d-2) & (v_today < v_7d-2))| (v_today<1)) {
    kleur <- paste("green")  ### green if value is lower than 7 days AND 14 days ago (with at least a margin of 2 per 100K) OR lower than 1 per 100K
  }
  
  kleur.table = rbind(kleur.table, data.frame(gemeente_getal=i,kleur=kleur))
  
  i <- i+1
}


RIVM_aantallen_gemeente_per_dag.combi.3 <- merge(combi.2, kleur.table)

RIVM_aantallen_gemeente_per_dag.combi.3 <- RIVM_aantallen_gemeente_per_dag.combi.3[RIVM_aantallen_gemeente_per_dag.combi.3$date>"2021-08-01",]


#RIVM_aantallen_gemeente_per_dag.combi.filter <- RIVM_aantallen_gemeente_per_dag.combi.3[RIVM_aantallen_gemeente_per_dag.combi.3$gemeente_Naam == "Oostzaan",]
#RIVM_aantallen_gemeente_per_dag.combi.filter <- RIVM_aantallen_gemeente_per_dag.combi.filter[RIVM_aantallen_gemeente_per_dag.combi.filter$date > "2020-11-01",]


ggplot(data= RIVM_aantallen_gemeente_per_dag.combi.3)+
  
#  geom_point(stat='identity', mapping = aes(x = date, y = phd), colour = "gray", size = 0.5)+
  
 # geom_smooth(mapping = aes(x = date, y = MAphd, colour = kleur), size =0.5, span = 0.3)+  
   geom_line(mapping = aes(x = date, y = MAphd, colour = kleur), size =0.6)+
  
  
#   scale_color_manual(values=c("green", "darkred",  "orange"))+
  scale_color_manual(values=c("green", "red", "darkred", "orange"))+
  
  
  #geom_bar(stat='identity',  fill = "#96afde")+
 # geom_line(color = "red")+
 # geom_point(size = 2)+
  
facet_wrap(~gemeente_Naam, )+ #  scales = "free_y")+
 
   scale_x_date(date_breaks = "1 month", 
              date_labels= format("%b"),
           limits = as.Date(c("2021-08-01", today)))+
  theme_bw() + 
  xlab("")+ 
  ylab("")+
  
  labs(title = "Alle gemeenten",
       subtitle = "Nieuwe gevallen, per 100.000 inwoners, 7-daags zwevend gemiddelde", # (Y-as wisselt)",
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
 
   theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
     legend.position = "none",   # no legend
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         
         axis.text = element_text(size=6,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=6),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.2, "cm"),
         #axis.line = element_line(colour = "#F5F5F5"),
         
         #panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
         ### facet label custom
         strip.text.x = element_text(size = 5, color = "black"),
         strip.background = element_rect(fill="gray"),  #, color="black"), #, size=0.2, linetype="solid"),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.y = element_blank(),
         panel.grid.minor.y = element_blank(),
  )+

ggsave("data/plots/75_Municipality-day-phd.png",width=13, height = 13)

