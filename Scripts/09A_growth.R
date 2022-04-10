


# growth

Merged_data_7MA <- Merged_data_2


#Gewenste dagen subsetten
Merged_data_short <- Merged_data_7MA[Merged_data_7MA$dateInTable>"2020-07-01"&Merged_data_7MA$dateInTable<=Sys.Date(),]
Merged_data_short$observation <- 1:nrow(Merged_data_short) 


Merged_data_short$fixedDate <- as.Date(Merged_data_short$dateInTable,format="%Y-%m-%d")


gf_c_last <- tail(Merged_data_short$gf_c, 1)




ggplot(Merged_data_short)+
  # geom_bar(stat='identity', mapping = aes(x=dateInTable, y=gf_c, fill = "x"))+     #, color = "#96afde"
  # scale_fill_manual(values=c("#96afde"))+

   geom_line(mapping = aes(x=fixedDate, y=0), color = "black",lwd = 2)+
   geom_line(mapping = aes(x=fixedDate, y=gf_c), color = "#F5F5F5",lwd = 4)+
   geom_line(mapping = aes(x=fixedDate, y=gf_c), color = "#44546a",lwd = 3)+
   
   theme_classic()+
   
   scale_y_continuous(limits = c(-100, 100), breaks = c(-25,-50,-75,-100, 0,25,50,75,100) )+ #,labels = (label_percent(1)) )+
   
   xlab("")+ 
   ylab("")+
   
   labs(title = paste("Groeifactor week-op-week nieuwe besmettingen is nu: ", gf_c_last, "%"),
        subtitle = "gebaseerd op het 7 daags voortschrijdend gemiddelde",
        caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+
   
   theme(
      plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
      panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
      legend.position = "none",   # no legend
      plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
      plot.subtitle =  element_text(hjust=0.5,size = 20,color = "black", face = "italic"),
      
      #scale_x_date(),
      
      axis.text = element_text(size=14,color = "black",face = "bold"),
      axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
      axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
      axis.ticks.length = unit(0.5, "cm"),
      axis.line = element_line(colour = "#F5F5F5"),
      panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed")
   )
ggsave("data/05_growth_cases.png",width=16, height = 9)





gf_d_last <- tail(Merged_data_short$gf_d, 1)

ggplot(Merged_data_short)+
   # geom_bar(stat='identity', mapping = aes(x=dateInTable, y=gf_c, fill = "x"))+     #, color = "#96afde"
   # scale_fill_manual(values=c("#96afde"))+
   
   
   geom_line(mapping = aes(x=fixedDate, y=0), color = "#000000",lwd = 2)+
   geom_line(mapping = aes(x=fixedDate, y=gf_d), color = "#44546a",lwd = 2)+
   
   
   #geom_xspline(aes(y = gf_d), size = 0.8,
    #            spline_shape = -.15, colour = 'red')
   
   
   theme_classic()+
   xlab("")+ 
   ylab("")+
   
   labs(title = paste("Groei week op week nieuwe overledenen is nu: ", gf_d_last, "%"),
        subtitle = "gebaseerd op het 7 daags voortschrijdend gemiddelde",
        caption = paste("Bron: RIVM | Plot: @YorickB ",Sys.Date()))+
   
   theme(
      plot.background = element_rect(fill = "#FDE3E3"), #background color/size (border color and size)
      panel.background = element_rect(fill = "#FDE3E3", colour = "#FDE3E3"),
      legend.position = "none",   # no legend
      plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
      plot.subtitle =  element_text(hjust=0.5,size = 20,color = "black", face = "italic"),
      
      #scale_x_date(),
      
      axis.text = element_text(size=14,color = "black",face = "bold"),
      axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
      axis.ticks = element_line(colour = "#FDE3E3", size = 1, linetype = "solid"),
      axis.ticks.length = unit(0.5, "cm"),
      axis.line = element_line(colour = "#FDE3E3"),
      panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed")
   )
ggsave("data/05_growth_dead.png",width=16, height =9)


