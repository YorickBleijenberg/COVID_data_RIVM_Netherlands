
library(tidyverse)


read.aantal.landelijk.path <- paste("C:\\Rdir\\rivm-data\\COVID-19_casus_landelijk_",Sys.Date()-1,".csv",sep="")
df_yesterday <- read.csv(read.aantal.landelijk.path,sep=";")


df_yesterday$weekOfDead<-substr(df_yesterday$Week_of_death,5,6)
df_yesterday$weekOfDead <- as.integer(df_yesterday$weekOfDead)
#df$weekOfDead <- as.Date(df$weekOfDead)  - we can convert to year-week later
df_yesterday_1 <- df_yesterday[!is.na(df_yesterday$weekOfDead), ]
df_yesterday_2 <- table(df_yesterday_1$weekOfDead)
df_yesterday_3 <-as.data.frame(df_yesterday_2)


read.aantal.landelijk.path <- paste("C:\\Rdir\\rivm-data\\COVID-19_casus_landelijk_",Sys.Date(),".csv",sep="")
df_today <- read.csv(read.aantal.landelijk.path,sep=";")

df_today$weekOfDead<-substr(df_today$Week_of_death,5,6)



df_today$weekOfDead <- as.integer(df_today$weekOfDead)
#df$weekOfDead <- as.Date(df$weekOfDead)  - we can convert to year-week later
df_today <- df_today[!is.na(df_today$weekOfDead), ]

df_today_2 <- table(df_today$weekOfDead)
df_today_3 <-as.data.frame(df_today_2)


df_merge <- merge(df_today_3, df_yesterday_3, by=c("Var1"))
colnames(df_merge) <- c("week","vandaag", "gisteren")


df_merge2 <-df_merge


df_merge3_diff <- df_merge2 %>% 
  select(week,vandaag,gisteren) %>%
  mutate(diff = vandaag - gisteren,
         )


df_merge3_diff$week  <-  levels(df_merge3_diff$week)[df_merge3_diff$week]

weeknumber<-strftime(Sys.Date(),format = "%V")
df_merge_3_short <- df_merge3_diff[df_merge3_diff$week>26&df_merge3_diff$week<=41,]



keycol <- "week"
valuecol <- "waardes"
gathercols <- c("gisteren", "diff")

df_merge_trans_long <- gather(df_merge_3_short, keycol, valuecol, gathercols)  #value = c("vandaag", "gisteren", "diff") # %>% 
 
#df_merge_trans_long_sort <- df_merge_trans_long[order(df_merge_trans_long$week),]





ggplot(df_merge_trans_long, aes(x=week, y=valuecol, fill=keycol))+
  geom_bar(stat="identity", position="stack")+
  
  theme_classic()+
  
  #geom_text(aes(label=valuecolvalue), vjust=-0.3, size=3.5)+
 # geom_text(
  #  aes(x = week, y = valuecol, label = valuecol, group = keycol),
  #  position = position_dodge(width = 1),
  #  vjust = -0.5, size = 10) + 
  
  xlab("")+ 
  ylab("")+
  
  #scale_x_date(date_breaks = "1 day", date_labels= format("%d-%b"),limits = as.Date(c(Sys.Date()-14, Sys.Date())))+
  
  scale_fill_manual( values=c("#ff0000", "#faacac"), labels=c("vandaag gemeld", "eerder gemeld"))+
                     
  labs(title = "Overleden personen",
       subtitle = "naar week van overlijden",
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme(legend.position = c(0.2, 0.9),
     legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
       legend.title = element_blank(),
        legend.text = element_text(colour="black", size=27, face="bold"))+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title = element_text(hjust = 0.5,size = 25,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text.x = element_text(face="bold", color="black", size=12), #, angle=45),
    axis.text.y = element_text(face="bold", color="black", size=14),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    #axis.labels.x=date_format("%d-%b"),
  
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))#,
  
ggsave("data/15_dead_diff.png",width=16, height = 9)  

