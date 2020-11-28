#### nertsen

#  70 besmet
#  56 niet besmet  (21 leeg -- 35 operationeel)



date.press = "2020-11-21"
nertsen.df <- data.frame(status = c("besmet", "leeg", "operationeel"), aantal = c(70,56,21))




ggplot(nertsen.df)+
  geom_col(aes(status, aantal, fill = status))+
  
  scale_fill_manual(values=c("darkred", "darkgreen", "orange"))+
  
  theme_classic() + 
  xlab("")+ 
  ylab("")+
  
  labs(title = "Nertsenbedrijven in Nederland",
       subtitle = paste("Status van de 126 bedrijven op:",date.press), # (Y-as wisselt)",
       caption = paste("Bron: Rijksoverheid | Plot: @YorickB | ",Sys.Date()))+
  
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
ggsave("data/80_mink.png",width=16, height = 9)


