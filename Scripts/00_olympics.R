require(tabulizer)
require(tidyverse)
require(lubridate)
require(scales) 
library(dplyr)
library(ggforce)

####
####  parse code by mzelst  ###
####


olympic.report<-"https://gtimg.tokyo2020.org/image/upload/production/zcf7dlrn4edbleqtfwit.pdf"


#### page 1 ####
area.olyimpics.table.total <- locate_areas(report,
                                          pages=c(1))

olyimpics.table <- extract_tables(report,
                           output = "data.frame",
                           pages = c(1),
                           area = area.olyimpics.table.total,
                           guess=FALSE)

olyimpics.table1 <- do.call(rbind,olyimpics.table)

colnames(olyimpics.table1) <- c("case.number","date.announce","date.report","resident", "quarantine","category", "prefecture","check", "notes")


olyimpics.table1 <-  (olyimpics.table1  %>% filter(case.number > "0"))

olyimpics.table1$category [olyimpics.table1$category==""]<-"Games-concerned personnel"
olyimpics.table1$check <- 1



#### page 2 ####
area.olyimpics.table.total <- locate_areas(report,
                                           pages=c(2))

olyimpics.table <- extract_tables(report,
                                  output = "data.frame",
                                  pages = c(2),
                                  area = area.olyimpics.table.total,
                                  guess=FALSE)

olyimpics.table2 <- do.call(rbind,olyimpics.table)

colnames(olyimpics.table2) <- c("case.number","date.announce","date.report","resident", "quarantine","category", "prefecture","check", "check2","notes")


olyimpics.table2 <-  (olyimpics.table2  %>% filter(case.number > "0"))

olyimpics.table2$category [olyimpics.table2$category==""]<-"Games-concerned personnel"
olyimpics.table2$check <- 1




#### page 3 ####
area.olyimpics.table.total <- locate_areas(report,
                                           pages=c(3))

olyimpics.table <- extract_tables(report,
                                  output = "data.frame",
                                  pages = c(3),
                                  area = area.olyimpics.table.total,
                                  guess=FALSE)

olyimpics.table3 <- do.call(rbind,olyimpics.table)

colnames(olyimpics.table3) <- c("case.number","date.announce","date.report","resident", "quarantine","category", "prefecture","check", "check2","notes")


olyimpics.table3 <-  (olyimpics.table3  %>% filter(case.number > "0"))

olyimpics.table3$category [olyimpics.table3$category==""]<-"Games-concerned personnel"
olyimpics.table3$check <- 1


#### page 4 ####
area.olyimpics.table.total <- locate_areas(report,
                                           pages=c(4))

olyimpics.table <- extract_tables(report,
                                  output = "data.frame",
                                  pages = c(4),
                                  area = area.olyimpics.table.total,
                                  guess=FALSE)

olyimpics.table4 <- do.call(rbind,olyimpics.table)

colnames(olyimpics.table4) <- c("case.number","date.announce","date.report","resident", "quarantine","category", "prefecture","check", "check2","notes")


olyimpics.table4 <-  (olyimpics.table4  %>% filter(case.number > "0"))

olyimpics.table4$category [olyimpics.table4$category==""]<-"Games-concerned personnel"
olyimpics.table4$check <- 1

olyimpics.table4 <- olyimpics.table4[-c(1), ]
olyimpics.table4$case.number <- as.numeric(olyimpics.table4$case.number)

#### page 5 ####
area.olyimpics.table.total <- locate_areas(report,
                                           pages=c(5))

olyimpics.table <- extract_tables(report,
                                  output = "data.frame",
                                  pages = c(5),
                                  area = area.olyimpics.table.total,
                                  guess=FALSE)

olyimpics.table5 <- do.call(rbind,olyimpics.table)

colnames(olyimpics.table5) <- c("case.number","date.announce","date.report","resident", "quarantine","category", "prefecture","check", "check2","notes")


olyimpics.table5 <-  (olyimpics.table5  %>% filter(case.number > "0"))

olyimpics.table5$category [olyimpics.table5$category==""]<-"Games-concerned personnel"
olyimpics.table5$check <- 1


#### page 6 ####
area.olyimpics.table.total <- locate_areas(report,
                                           pages=c(6))

olyimpics.table <- extract_tables(report,
                                  output = "data.frame",
                                  pages = c(6),
                                  area = area.olyimpics.table.total,
                                  guess=FALSE)

olyimpics.table6 <- do.call(rbind,olyimpics.table)

colnames(olyimpics.table6) <- c("case.number","date.announce","date.report","resident", "quarantine","category", "prefecture","check", "check2")


olyimpics.table6 <-  (olyimpics.table6  %>% filter(case.number > "0"))

olyimpics.table6$category [olyimpics.table6$category==""]<-"Games-concerned personnel"
olyimpics.table6$check <- 1



#### bind df ####

olyimpics.table.total <- bind_rows(olyimpics.table1,olyimpics.table2)
olyimpics.table.total <- bind_rows(olyimpics.table.total,olyimpics.table3)
olyimpics.table.total <- bind_rows(olyimpics.table.total,olyimpics.table4)
olyimpics.table.total <- bind_rows(olyimpics.table.total,olyimpics.table5)
olyimpics.table.total <- bind_rows(olyimpics.table.total,olyimpics.table6)

olyimpics.table.total$date.report <- as.Date(olyimpics.table.total$date.report, "%d-%b-%y")

#### write file ####
olympic.file <- paste0("data/olympics/olympics_", Sys.Date(), ".csv")
write.csv2(olyimpics.table.total, file = olympic.file, row.names = F)



#e69f00
#56b4e9
#009e73
#f0e442
#0072b2
#d55300
#cc79a7
#000000

# IBM

#648fff
#785ef0
#dc267f
#fe6100
#ffb000



#### plot olympics  #####
ggplot(olyimpics.table.total, aes(x=date.report, y=check, fill = factor(category, levels=c("Media",
                                                                                           "Tokyo 2020 volunteers",
                                                                                           "Tokyo 2020 Contractor",
                                                                                           "Tokyo 2020 employee",
                                                                                           "Games-concerned personnel",
                                                                                           "Athletes"
                                                                                           ))
                                                                                           )) +
    geom_bar(stat='identity')+
  
  scale_fill_manual(values = c("#e69f00", "#56b4e9", "#009e73","#f0e442", "#0072b2", "#d55300" ))+ 
  
  theme_classic()+
  xlab("Date")+ 
  ylab("Number of cases")+
  labs(title = "Olympics: Covid-19 Positive Cases",
       subtitle = "By category",
       caption = paste("Source: tokyo2020.org | Plot: @YorickB | ",Sys.Date()))+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
  theme(legend.position = "right", # c(0.25, 0.90),
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
  
  
ggsave("data/plots/00_olympics.png",width=16, height = 9)




#### page last ####
area.olyimpics.table.total <- locate_areas(olympic.report,
                                           pages=c(9))

olyimpics.table <- extract_tables(olympic.report,
                                  output = "data.frame",
                                  pages = c(9),
                                  area = area.olyimpics.table.total,
                                  guess=FALSE)

olyimpics.table9 <- do.call(rbind,olyimpics.table)

olyimpics.table9 <- olyimpics.table9[,-c(2,5,7,10,11,13)]
olyimpics.table9 <- olyimpics.table9[-c(1,2),]

colnames(olyimpics.table9) <- c("date.announce","Athlete", "Games concerned personnel", "Media",
                                "Tokyo 2020 Employee", "Tokyo 2020 Contractor","Tokyo 2020 Volunteer", 
                                "total","Olymipc.Village","outside")


olympic.file <- paste0("data/olympics/olympics_table_", Sys.Date(), ".csv")
write.csv(olyimpics.table9, file = olympic.file, row.names = F)



key <- "date.announce"
value <- "total"
olyimpics.table.long <- gather(olyimpics.table9, key, value, 2:7)
olyimpics.table.long$value <- as.numeric(olyimpics.table.long$value)
olyimpics.table.long$date.announce <- as.Date(olyimpics.table.long$date.announce, "%d-%b-%y" )



ggplot(olyimpics.table.long, aes(x=date.announce, y=value, fill = factor(key, levels=c("Media",
                                                                                       "Tokyo 2020 Volunteer",
                                                                                            "Tokyo 2020 Contractor",
                                                                                            "Tokyo 2020 Employee",
                                                                                            "Games concerned personnel",
                                                                                            "Athlete")) )) +
         geom_bar(stat='identity')+
  
  scale_fill_manual(values = c("#e69f00", "#56b4e9", "#009e73","#f0e442", "#0072b2", "#d55300" ))+ 
    
 # scale_x_date( limits = c(as.Date("2020-07-02"), NA), breaks = "week",  labels = date_format("%V"))+

  
  theme_classic()+
  xlab("Date")+ 
  ylab("Number of cases")+
  labs(title = "Olympics: Covid-19 Positive Cases",
       subtitle = "By category",
       caption = paste("Source: tokyo2020.org | Plot: @YorickB | ",Sys.Date()))+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
  theme(legend.position = "right", # c(0.25, 0.90),
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
  
  
ggsave("data/plots/00_olympics.table.png",width=16, height = 9)

