




data.jan<- readRDS("C:\\Rdir\\data\\Aantalback_2021-11-24.rds")


data.jan2 <- data.jan[,-c(1)]


# dagvdata
# lftdata
# n

data.jan.spread <- spread(data.jan2, lftdata, n)


data.jan.spread$`0` <-  rollmean(data.jan.spread$`0`, 7, align='right',fill=NA)  
data.jan.spread$`1` <-  rollmean(data.jan.spread$`1`, 7, align='right',fill=NA)  
data.jan.spread$`2` <-  rollmean(data.jan.spread$`2`, 7, align='right',fill=NA)  
data.jan.spread$`3` <-  rollmean(data.jan.spread$`3`, 7, align='right',fill=NA)  
data.jan.spread$`4` <-  rollmean(data.jan.spread$`4`, 7, align='right',fill=NA)  
data.jan.spread$`5` <-  rollmean(data.jan.spread$`5`, 7, align='right',fill=NA)  
data.jan.spread$`6` <-  rollmean(data.jan.spread$`6`, 7, align='right',fill=NA)  
data.jan.spread$`7` <-  rollmean(data.jan.spread$`7`, 7, align='right',fill=NA)  
data.jan.spread$`10` <-  rollmean(data.jan.spread$`10`, 7, align='right',fill=NA)  
data.jan.spread$`20` <-  rollmean(data.jan.spread$`20`, 7, align='right',fill=NA)  


data.jan.spread$`1` <- shift(data.jan.spread$`1`, -1)
data.jan.spread$`2` <- shift(data.jan.spread$`2`, -2)
data.jan.spread$`3` <- shift(data.jan.spread$`3`, -3)
data.jan.spread$`4` <- shift(data.jan.spread$`4`, -4)
data.jan.spread$`5` <- shift(data.jan.spread$`5`, -5)
data.jan.spread$`6` <- shift(data.jan.spread$`6`, -6)
data.jan.spread$`7` <- shift(data.jan.spread$`7`, -7)
data.jan.spread$`10` <- shift(data.jan.spread$`10`, -10)
data.jan.spread$`20` <- shift(data.jan.spread$`20`, -20)

keycol <- "dagvdata"
valuecol <- "lftdata"
gathercols <- c("0","1","2","3", "4","5","6","7", "10","20")
data.jan.gather <- gather(data.jan.spread, keycol, valuecol, gathercols)
colnames(data.jan.gather) <- c("dagvdata", "lftdata", "ma")

data.jan.gather <- data.jan.gather[order(as.Date(data.jan.gather$dagvdata)),]

#data.jan.gather$data.jan.gather<-factor(data.jan.gather$lftdata,levels = c("20","10","7","6","5","4","3","2","1","0"))

legent.text2 <- "Leeftijd data"

ggplot(data = data.jan.gather) + 
 
  geom_line(mapping = aes(x = dagvdata, y = ma, color=lftdata))+
  geom_point(mapping = aes(x = dagvdata, y = ma, color=lftdata))+
  
  scale_x_date(breaks = date_breaks("1 weeks"),
    limits = as.Date(c("2020-10-18", "2021-01-18")))+

  geom_vline(xintercept = as.Date("2020-12-15"))+
  geom_vline(xintercept = as.Date("2020-12-21"),  linetype = "dotted")+
  
ggsave("data/99_jan_data_ma_lockdown.png",width=16, height = 9)


ggplot(data = data.jan.gather) + 
  
  geom_line(mapping = aes(x = dagvdata, y = ma, color=lftdata))+
  geom_point(mapping = aes(x = dagvdata, y = ma, color=lftdata))+
  
  scale_x_date(breaks = date_breaks("1 weeks"),
               limits = as.Date(c("2021-05-18", "2021-08-18")))+
  
  geom_vline(xintercept = as.Date("2021-07-09"))+
  geom_vline(xintercept = as.Date("2021-07-16"),  linetype = "dotted")+
  
ggsave("data/99_jan_data_ma_dmj.png",width=16, height = 9)




ggplot(data = data.jan.gather,  aes(color = factor(lftdata, levels=c("20","10","7","6","5","4","3","2","1","0")))) + 
  
  geom_line(mapping = aes(x = dagvdata, y = ma))+
  geom_point(mapping = aes(x = dagvdata, y = ma))+
  
  scale_x_date(breaks = date_breaks("1 weeks"),
               limits = as.Date(c("2021-7-18", NA)))+
  
  geom_vline(xintercept = as.Date("2021-11-12"))+
  geom_vline(xintercept = as.Date("2021-11-19"),  linetype = "dotted")+
  
  labs(title = "Harde klap",
       subtitle = "Ontwikkeling aantal casussen per dag",
       caption = paste("Bron: RIVM | idea by jannoTR |  Plot: @YorickB | ",Sys.Date()))+
  
  theme(legend.position = "right" ,
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
  
ggsave("data/99_jan_data_ma_klap.png",width=16, height = 9)







ggplot(data = data.jan) + 
  
  geom_line(mapping = aes(x = Date_statistics, y = n, color=lftdata))+
  geom_point(mapping = aes(x = Date_statistics, y = n, color=lftdata))+
  
    scale_x_date(limits = as.Date(c("2021-07-18", NA)))+

  
ggsave("data/99_jan_data.png",width=16, height = 9)

  