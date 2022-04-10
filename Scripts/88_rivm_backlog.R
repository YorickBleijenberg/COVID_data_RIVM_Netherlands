



today = Sys.Date()


rivm.backlog.file <-paste0("C:\\Rdir\\data\\",Sys.Date()-1,"\\", Sys.Date()-1,"_rivm_backlog.csv")
rivm.backlog <- read.csv(rivm.backlog.file,sep=",")
colnames(rivm.backlog) = c("date", "backlog")
rivm.backlog$date = as.Date(rivm.backlog$date)


rivm.backlog[nrow(rivm.backlog) + 1,] = data.frame(today, current.backlog)

rivm.backlog.file2 <-    paste("C:\\Rdir\\data\\",Sys.Date(),"\\", Sys.Date(), "_rivm_backlog.csv", sep = "")
write.csv(rivm.backlog, file = rivm.backlog.file2, row.names = F)



rivm.backlog.working <- rivm.backlog


rivm.backlog.working$ma.backlog <- rollmeanr(rivm.backlog.working$backlog, 7, fill = 0)
rivm.backlog.working$ma.backlog_lead  <- lead(rivm.backlog.working$ma.backlog,3)


ggplot(rivm.backlog.working)+
  
  geom_bar(stat='identity', mapping = aes(x=date, y=backlog))+
  geom_line( mapping = aes(x=date, y=ma.backlog), size = 5, color = "#F5F5F5")+
  geom_line( mapping = aes(x=date, y=ma.backlog), size = 4)+
           
  
theme_classic()+
  xlab("")+ 
  ylab("")+

    labs(title = "RIVM backlog",
         caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date())
         )+
  
  scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"),
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    legend.position = "none",   
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
ggsave("data/88_rivm_backlog.png",width=16, height = 9)


#### tweet prep ####
deE <- intToUtf8(0x00EB)


Date <-  Sys.Date()
Dateyesterday <-  Sys.Date()-1
DateAweekAgo <-  Sys.Date()-7

Working_Set_rivm <- subset(rivm.backlog.working, date ==Date | date ==DateAweekAgo | date ==Dateyesterday )





a <- Working_Set_rivm$backlog[3]   # today
b <- Working_Set_rivm$backlog[2]   # yesterday
c <- Working_Set_rivm$backlog[1]   # last week




#### New cases Tweet ####


diff.backlog.day <- abs(a-b)     ####
diff.backlog.week <- abs(a-c)    ####



if (a < b) {
  more.less.day.case <- paste("minder",intToUtf8(0x2B07), "dan gisteren.")
} else if (a > b) {
  more.less.day.case <- paste("meer",intToUtf8(0x2197), "dan gisteren.")
} else
  more.less.day.case <- paste("meer", intToUtf8(0x2194),"dan gisteren. (gelijk)")

if (a < c) {
  more.less.week.case <- paste("minder",intToUtf8(0x2B07), "dan een week geleden.")
} else if (a > c) {
  more.less.week.case <- paste("meer",intToUtf8(0x2197), "dan een week geleden.")
} else
  more.less.week.case <- paste("meer", intToUtf8(0x2194),"dan een week geleden. (gelijk)")


if (a < b) {
  more.less.day.case.dot <- intToUtf8(0x1F7E2)
} else if (a > b) {
  more.less.day.case.dot <- intToUtf8(0x1F534)
} else
  more.less.day.case.dot <- intToUtf8(0x1F7E1)

if (a < c) {
  more.less.week.case.dot <- intToUtf8(0x1F7E2)
} else if (a > c) {
  more.less.week.case.dot <- intToUtf8(0x1F534)
} else
  more.less.week.case.dot <- intToUtf8(0x1F7E1)







a  <- format( a, big.mark="." ,decimal.mark=",")
diff.backlog.day  <- format( diff.backlog.day, big.mark="." ,decimal.mark=",")
diff.backlog.week  <- format( diff.backlog.week, big.mark="." ,decimal.mark=",")







#### tweet.cases.tweet ####

tweet.rivm.backlog.tweet <- "Registratie achterstand bij het RIVM:

%s uitslagen moeten nog worden verwerkt.

Indicatoren (exponenti%sle) groei / krimp:
%s Dat is %s %s
%s Dat is %s %s 
"


tweet.rivm.backlog.tweet <- sprintf(tweet.rivm.backlog.tweet,
                             a, deE,
                             more.less.day.case.dot,  diff.backlog.day,    more.less.day.case,
                             more.less.week.case.dot, diff.backlog.week,   more.less.week.case
                            )
Encoding(tweet.rivm.backlog.tweet) <- "UTF-8"

post_tweet(tweet.rivm.backlog.tweet,  media = c("data/88_rivm_backlog.png"), in_reply_to_status_id = get_reply_id())

           