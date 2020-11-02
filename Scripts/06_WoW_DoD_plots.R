library(tidyverse)


#library(dplyr)
#  test.d.f <-read.csv("C:\\Rdir\\data\\test.csv",sep=";")

#test.d.f <- Merged_data
test.d.f <- Merged_data[Merged_data$dateInTable>"2020-07-01"&Merged_data$dateInTable<=Sys.Date(),]

#  Merged_data_short <- Merged_data[Merged_data$dateInTable>"2020-07-01"&Merged_data$dateInTable<=Sys.Date(),]
#transform(test.d.f, diff_qsec = c(NA, diff(varA)))
#test.d.f
#transform(test.d.f, diff_qsec = c(NA, diff(varA)))
#diff_varA.1 <- test.d.f %>% mutate(diff_varA = varA - lag(varA)) # dplyr has its own version of lag
#test.d.f.1 <- test.d.f
#test.d.f.1$diff <- diff_varA.1
#test.d.f.2 <- test.d.f.1
#test.d.f.2$diffAbs <- ifelse(test.d.f.1$diff < 0, "negative","positive")
#test.d.f.2

#  diff_varA.1 <- test.d.f.2 %>% mutate(diff_varA = varA - lag(varA))

#test.d.f.3 <- diff(test.d.f$varA, lag = 1)
#x <- c(0)
#test.d.f$seday <- append(x, test.d.f.3)


test.d.f.2 <- test.d.f

diff_date.1 <- diff(test.d.f.2$cases, lag = 7)
x <- c(0,0,0,0,0,0,0)
seday <- append(x, diff_date.1)
test.d.f.3 <- test.d.f.2

test.d.f.3$seday <- seday

colour <- ifelse(test.d.f.3$seday < 0, "negative","positive")

test.d.f.3$diffAbs <- colour




test.d.f.3$dateInTable <- as.Date(test.d.f.3$dateInTable,format="%Y-%m-%d")



ggplot(test.d.f.3, aes(x = dateInTable, y = seday, fill = colour)) +
  geom_col(position = "identity")+
  scale_fill_manual(values = c("#4472c4", "#ba5800"), guide = FALSE)+
  
  labs(title = "Besmettingen: Verandering week op week",
       #subtitle = "", fill="Week",
       caption = paste("Data: RIVM | plot door @YorickB ",Sys.Date()))+
  xlab("")+ 
  ylab("")+
  theme_classic()+  #base_size = 20)+
  
  theme(axis.title.x=element_blank(),
        axis.text = element_text(size=14,color = "black",face = "bold"),
        axis.ticks.x=element_blank())+
  
  theme(
    plot.background = element_rect(fill = "#f5f5f5"),
    panel.background = element_rect(fill = "#f5f5f5", colour = "#f5f5f5", size = 0.5, linetype = "solid"),
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    #axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    )
ggsave("data/08_new_cases_WoW.png",width=16, height = 9)  



ggplot(test.d.f.3, aes(x = dateInTable, y = seday, fill = colour)) +
  geom_col(position = "identity")+
  scale_fill_manual(values = c("#4472c4", "#ba5800"), guide = FALSE)+
  
  labs(title = "Cases: week on week change",
       #subtitle = "", fill="Week",
       caption = paste("Source: RIVM | plot: @YorickB | ",Sys.Date()))+
  xlab("")+ 
  ylab("")+
  theme_classic()+  #base_size = 20)+
  
  theme(axis.title.x=element_blank(),
        axis.text = element_text(size=14,color = "black",face = "bold"),
        axis.ticks.x=element_blank())+
  
  theme(
    plot.background = element_rect(fill = "#f5f5f5"),
    panel.background = element_rect(fill = "#f5f5f5", colour = "#f5f5f5", size = 0.5, linetype = "solid"),
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    #axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
  )
ggsave("data/08_EN_new_cases_WoW.png",width=16, height = 9)  


  
  
 # theme(plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
 #       panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
  #      plot.title = element_text(hjust = 0.5,size = 20,family  = "Corbel",face = "bold"),
  #      plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic",family  = "Corbel"),
  #      axis.text = element_text(size=14,color = "black", family  = "Corbel",face = "bold"),
 #       axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
 #       
  #      panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+





#===============================================================



test.d.f.2 <- test.d.f


test.d.f.2$dateInTable <- as.Date(test.d.f.2$dateInTable,format="%Y-%m-%d")


diff_date.1 <- diff(test.d.f.2$cases, lag = 1)
x <- c(0)
seday <- append(x, diff_date.1)
test.d.f.3 <- test.d.f.2

test.d.f.3$seday <- seday

colour <- ifelse(test.d.f.3$seday < 0, "negative","positive")

test.d.f.3$diffAbs <- colour

ggplot(test.d.f.3, aes(x = dateInTable, y = seday, fill = colour)) +
  geom_col(position = "identity")+
  scale_fill_manual(values = c("#4472c4", "#ba5800"), guide = FALSE)+
  
  labs(title = "Besmettingen: Verandering dag op dag",
       #subtitle = "", fill="Week",
       caption = paste("Data: RIVM | plot door @YorickB ",Sys.Date()))+
  
  theme_classic()+
  
  xlab("")+ 
  ylab("")+
  
  #theme_classic()+  #base_size = 20)+
  
  theme(axis.title.x=element_blank(),
        axis.text = element_text(size=14,color = "black",face = "bold"),
        axis.ticks.x=element_blank())+
  
  theme(
    plot.background = element_rect(fill = "#f5f5f5"),
    panel.background = element_rect(fill = "#f5f5f5", colour = "#f5f5f5", size = 0.5, linetype = "solid"),
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    #axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    
    panel.grid.major.y = element_line(colour= "black", linetype = "dashed"),
    panel.grid.minor.y = element_line(colour= "black", linetype = "dashed"),
    
  )
ggsave("data/07_new_cases_DoD.png",width=16, height = 9)  

