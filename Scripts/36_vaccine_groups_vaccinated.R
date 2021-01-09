
library(scales)

#today <- Sys.Date()
#vaccine.groups.data <- read.csv("C:\\Rdir\\data-contstant\\vaccine.groups.data.csv" ,sep=";")
#colnames(vaccine.groups.data) = c("vac.group",	"number.in.group",	"number.vac.in.group",	"vaccin.type")
vaccine.groups.data <-read.csv("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/vaccination/people.vaccinated-groups.csv",sep=",")
#people.vaccinated.groups.gh$date <- as.Date(people.vaccinated.groups.gh$date)

vaccine.groups.data$percentage <- round(((vaccine.groups.data$number.vac.in.group/vaccine.groups.data$number.in.group)*100), digits=2)
vaccine.groups.data$not.vac.in.group <- vaccine.groups.data$number.in.group-vaccine.groups.data$number.vac.in.group

keycol <- "vac.group"
valuecol <- "data"
gathercols <- c("not.vac.in.group","number.vac.in.group")

vaccine.groups.data.gather <- gather(vaccine.groups.data, keycol, valuecol, gathercols)

#### group plo ####

ggplot(vaccine.groups.data, aes(x=percentage, y=""))+
geom_col(fill = "darkgreen")+
  facet_wrap(~vac.group,ncol=1)+
  scale_x_continuous(limits = c(0, 100))+
  
#scale_fill_manual(values=c("#96afde"))+
 
   theme_classic()+
  xlab("")+ 
  ylab("")+
#  scale_y_continuous( labels = label_comma(big.mark = ".", decimal.mark = ","))+
  labs(title = "Doelgroepen gevaccineerd",
       subtitle = "percentage of the target group that is vaccinated",
       caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+
  theme(
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    legend.position = "none",   # no legend
    plot.title = element_text(hjust = 0.5,size = 40,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic"),
    #scale_x_date(),
    axis.text = element_text(size=14,color = "black",face = "bold"),
    #axis.text.x=element_blank(),
    axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    ### facet label custom
    strip.text.x = element_text(size = 14, color = "black"),
    strip.background = element_rect(fill="gray"),  #, color="black"), #, size=0.2, linetype="solid"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),) #,
    #panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))

ggsave("data/91_vaccine_to_groups.png",width=9, height = 16)


  
#   geom_col(position = "dodge") #, width = 1, fill = "Steelblue") 
   
#geom_bar(aes(fill = keycol), position = position_stack(reverse = TRUE)) 



  
   #,labels = label_comma(big.mark = ".", decimal.mark = ","))
  
