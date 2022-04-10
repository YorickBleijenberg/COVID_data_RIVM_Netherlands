
library(zoo)
library(tidyverse)
library(plotrix)

Merged_data_7MA <- Merged_data_2


#Gewenste dagen subsetten
Merged_data_short <- Merged_data_7MA[Merged_data_7MA$dateInTable>"2020-07-01"&Merged_data_7MA$dateInTable<=Sys.Date(),]
Merged_data_short$observation <- 1:nrow(Merged_data_short) 


Merged_data_short$fixedDate <- as.Date(Merged_data_short$dateInTable,format="%Y-%m-%d")


test.date.df =data.frame(date=as.Date(c("2020-12-01")),event="verruiming testbeleid")



persco.df=data.frame(date=as.Date(c("2020-10-14",   "2020-12-15", "2020-12-25", "2021-01-01", "2021-01-12","2021-01-19")), 
                     event=c("Semi-lockdown",   "lockdown", "kerst", "1 jan", "persco", "einde lockdown?"))

persco.df.2=data.frame(date=as.Date(c("2020-12-25", "2021-01-01", "2021-01-12","2021-01-19")), 
                     event=c( "kerst", "1 jan", "persco", "einde lockdown?"))


new.cases.title.number  <- format( (last(Merged_data_7MA$cases)), big.mark="." ,decimal.mark=",")
new.cases.title <-  paste("Nieuw gemelde besmettingen  -  ", new.cases.title.number )








ggplot(Merged_data_short)+
    geom_bar(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"))+     #, color = "#96afde"
    
   
        scale_fill_manual(values=c("#96afde"))+
    geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#F5F5F5",lwd = 3)+
    geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#44546a",lwd = 2)+
  
    
    scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
    theme_classic()+
    xlab("")+ 
    ylab("")+
    labs(title = new.cases.title,
         subtitle = "met 7 daags voortschrijdend gemiddelde",
         caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
    theme(
        plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        legend.position = "none",   # no legend
        plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
       #scale_x_date(),
        axis.text = element_text(size=14,color = "black",face = "bold"),
       #axis.text.x=element_blank(),
        axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.5, "cm"),
        axis.line = element_line(colour = "#F5F5F5"),
        panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))
ggsave("data/plots/05_new_cases.png",width=16, height = 9)





ggplot(Merged_data_short)+
    geom_bar(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"))+     #, color = "#96afde"
    scale_fill_manual(values=c("#96afde"))+
    geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#F5F5F5",lwd = 3)+
    geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#44546a",lwd = 2)+
    theme_classic()+
    xlab("")+ 
    ylab("")+
    scale_y_continuous(limits = c(1024, 33000), trans='log2', labels = label_comma(big.mark = ".", decimal.mark = ","))+
    labs(title = "Nieuw gemelde besmettingen, logaritmisch",
         subtitle = "met 7 daags voortschrijdend gemiddelde",
         caption = paste("Bron: RIVM | Plot: @YorickB  | ",Sys.Date()))+
    theme(
        plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        legend.position = "none",   # no legend
        plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        #scale_x_date(),
        axis.text = element_text(size=14,color = "black",face = "bold"),
       # axis.text.x=element_blank(),
        axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.5, "cm"),
        axis.line = element_line(colour = "#F5F5F5"),
        panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))
ggsave("data/06_new_cases_log_line.png",width=16, height = 9)









ggplot(Merged_data_short)+
    geom_bar(stat='identity', mapping = aes(x=fixedDate, y=dead, fill = "x"))+     #, color = "#96afde"
    scale_fill_manual(values=c("#fab0b0"))+
    geom_line(mapping = aes(x=fixedDate, y=ma_d_lead), color = "#F5F5F5",lwd = 3)+
    geom_line(mapping = aes(x=fixedDate, y=ma_d_lead), color = "#ff0505",lwd = 2)+
    theme_classic()+
    xlab("")+ 
    ylab("")+
    labs(title = "Nieuw gemelde overledenen",
         subtitle = "met 7 daags voortschrijdend gemiddelde",
         caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
    theme(
        plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
        panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        legend.position = "none",   # no legend
        plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
        #scale_x_date(),
       axis.text = element_text(size=14,color = "black",face = "bold"),
       # axis.text.x=element_blank(),
        axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
        axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
        axis.ticks.length = unit(0.5, "cm"),
        axis.line = element_line(colour = "#F5F5F5"),
        panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))
ggsave("data/13_new_deceased.png",width=16, height = 9)



daily.numbers.filename <- paste0("data/plots/",format(Sys.Date(), "%Y-%m-%d"), "_daily_numbers.csv")
write.csv2(Merged_data_7MA, file = daily.numbers.filename, row.names = F)


#### doubling check cases ####
Merged_data_short_2021 <- Merged_data_short[Merged_data_short$fixedDate>"2021-06-01",]


minMA_summer2021 <- min(Merged_data_short_2021$MACases)
current_MA <- last(Merged_data_short_2021$MACases)

#minMA_summer2021 <- 600
#current_MA <- 1100

increase1a <- (current_MA/minMA_summer2021)*100
increase1a <- round(increase1a, digits =0)


doubling1a <- log2(current_MA/minMA_summer2021)
doubling1a <- round(doubling1a, digits =2)


new.cases.subtitle <- paste0("Stijging van het 7 daags voortschrijdend gemiddelde tov het dal: ", increase1a , "%\n",
                             "Dat is: ", doubling1a, " verdubbeling")

ggplot(Merged_data_short_2021)+

    geom_bar(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"))+
  
  scale_fill_manual(values=c("#96afde"))+
  geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#F5F5F5",lwd = 3)+
  geom_line(mapping = aes(x=fixedDate, y=ma_c_lead), color = "#44546a",lwd = 2)+
  
  scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
theme_classic()+
  xlab("")+ 
  ylab("")+
  labs(title = new.cases.title,
       subtitle =  "met 7 daags voortschrijdend gemiddelde",
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), 
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    legend.position = "none", 
    plot.title    =  element_text(hjust = 0.5 , size = 30, face = "bold"),
    plot.subtitle =  element_text(hjust = 0.5 , size = 15, color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14), 
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))
ggsave("data/plots/05_new_cases_2021.png",width=16, height = 9)





# get some extra room on the left
par(mar=c(5,5,4,2))
# make up some happiness data, as so many seem to do
happyday<-data.frame(Monday=c(2.3,3.4),Tuesday=c(2.8,3.3),Wednesday=c(3.2,3.1),
                     Thursday=c(3.6,2.8),Friday=c(4.2,2.6),Saturday=c(4.5,2.9),Sunday=c(4.1,2.8))
happylabels<-c("Utterly dashed","Rather mopey","Indifferent","Somewhat elated",
               "Euphoric")

barp(happyday,
     #names.arg=names(happyday),
     legend.lab=c("Slaves","Unemployed"),
     legend.pos=list(x=2,y=4.5),
     col=c("#ee7700","#3333ff"),
     main="9AM happiness by weekday",
     xlab="Day of week",
     ylab="Happiness rating",
     ylim=c(1,5),
     staxx=TRUE,
     staxy=TRUE,
     height.at=1:5,
     height.lab=happylabels,
     cex.axis=0.9,
     cylindrical=TRUE,
     shadow=TRUE)






ggplot(Merged_data_short)+
  geom_bar(stat='identity', mapping = aes(x=fixedDate, y=cases, fill = "x"))



Merged_data_short_trans <- Merged_data_short[,-c(3:15)]
Merged_data_short_trans <- spread(Merged_data_short_trans, dateInTable, cases)


barp(Merged_data_short_trans ,
     axis.break(axis=2,breakpos=2,pos=4,bgcol="white",breakcol="black",
                style="slash",brw=0.02))
     
          

#  axis.break(2, 2.9, style = "zigzag"))

#     axis.break(100000, 250000, style = "zigzag"))

#  100000, 350000







# now do a plot with colors scaled to the sex ratio (real data!)
sexratio<-c(0.24,0.35,0.09,0.59,0.63,0.34,0.7,0.6)
# the fun ratings are once again a pack of lies
funrating<-c(3.2,3.5,1.5,5.4,4.5,2.7,6.8,4.9)
funstudy<-c("Astronomy","Chemistry","Economics","Anthropology","Linguistics",
            "Math/Stats","Psychology","Sociology")
funlabels<-c("Torture","Agony","Boredom","Neutral","Entertaining","Exhilarating",
             "Maniacal")
# xrange is used to get the colors to match the 0-100% scale
barp(funrating,names.arg=funstudy,main="Fun ratings for various areas of study",
     col=color.scale(sexratio,c(0.2,1),c(0.2,0.4),c(1,0.4),xrange=c(0,1)),
     xlab="Study",ylab="Rating",height.at=1:7,height.lab=funlabels,ylim=c(1,7),
     staxx=TRUE,staxy=TRUE,cex.axis=0.9)
# here we want the full scale from zero to one
color.legend(2,6,4,6.4,legend=c("100% guys","100% girls"),
             rect.col=color.scale(seq(0,1,by=0.25),c(0.2,1),c(0.2,0.4),c(1,0.4)))
par(mar=c(5,4,4,2))
# use barp to display a multiple histogram with a shaded background
# notice how the expression uses local variables inside the barp function
gradbg<-"gradient.rect(xlim[1],ylim[1],xlim[2],ylim[2],
  c(1,0.5,1),c(1,0.5,1),c(1,0.5,1),gradient=\"y\",nslices=100)"
h1<-table(cut(rnorm(100,4),breaks=seq(0,8,by=2)))
h2<-table(cut(rnorm(100,4),breaks=seq(0,8,by=2)))
h3<-table(cut(rnorm(100,4),breaks=seq(0,8,by=2)))
hmat<-matrix(c(h1,h2,h3),nrow=3,byrow=TRUE)
barp(hmat,names.arg=names(h1),width=0.45,col=2:4,do.first=gradbg,
     main="Multiple histogram using barp",xlab="Bins",ylab="Frequency")
legend(3.8,50,c("h1","h2","h3"),fill=2:4)
# now display a positive/negative plot
barp(c(2,-3,4,-5,6,-7,8),main="Positive/negative plot",
     xlab="Alternating colors",ylab="For alternating values",
     col=2+(c(2,-3,4,-5,6,-7,8)>0))


