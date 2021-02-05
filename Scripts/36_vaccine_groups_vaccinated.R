
library(scales)

today = Sys.Date()

vaccine.groups.data =1
#today <- Sys.Date()
#vaccine.groups.data <- read.csv("C:\\Rdir\\data-contstant\\vaccine.groups.data.csv" ,sep=";")
#colnames(vaccine.groups.data) = c("vac.group",	"number.in.group",	"number.vac.in.group",	"vaccin.type")
vaccine.groups.data <-read.csv("https://raw.githubusercontent.com/YorickBleijenberg/COVID_data_RIVM_Netherlands/master/vaccination/people.vaccinated-groups.csv",sep=",")
vaccine.groups.data$date <- as.Date(vaccine.groups.data$date)

vaccine.groups.data <- vaccine.groups.data[vaccine.groups.data$date==today,]

vaccine.groups.data$percentage <- round(((vaccine.groups.data$number.vac.in.group/vaccine.groups.data$number.in.group)*100), digits=2)
vaccine.groups.data$not.vac.in.group <- vaccine.groups.data$number.in.group-vaccine.groups.data$number.vac.in.group

vaccine.groups.data$hunderd.P <- 100

 vaccine.groups.data$vac.group <- c(2	,3	,6	,1	,5	,7	,4	,10	,8	,9,11,12,13,14,15,16,17	)



keycol <- "vac.group"
valuecol <- "data"
gathercols <- c("hunderd.P","percentage")
vaccine.groups.data.gather.perc <- gather(vaccine.groups.data, keycol, valuecol, gathercols)


    vaccine.groups.data$number.vac.in.group[vaccine.groups.data$number.vac.in.group == 0] <-  "NA"

    
    
    vaccine.groups.data.gather <-1
    
keycol <- "order"
valuecol <- "data"
gathercols <- c("number.in.group","number.vac.in.group")
vaccine.groups.data.gather <- gather(vaccine.groups.data, keycol, valuecol, gathercols)

vaccine.groups.data.gather$valuecol <- as.integer(vaccine.groups.data.gather$valuecol)

vaccine.groups.data.gather$vac.group <- as.factor(vaccine.groups.data.gather$vac.group)

levels(vaccine.groups.data.gather$vac.group) <- c("Bewoners verpleeghuizen en mensen met een verstandelijke beperking",
                                                  "Medewerkers acute zorg",
                                                  "Medewerkers verpleeghuizen & gehandicaptenzorg & wijkverpleging",
                                                  "18-60 medische indicatie",
                                                  "Bewoners intramurale GGZ",
                                                  "Thuiswonenden van 60-75",
                                                  "Thuiswonenden 75+, en niet mobiele 60+",
                                                  "Medewerkers GGZ",
                                                  "Medewerkers zorg overig",
                                                  "De rest (muv kinderen)"
)

#### group absolute plot ####

  ggplot(vaccine.groups.data.gather,aes(x = valuecol, y = "", fill = keycol,
                                        color = keycol, 
                                        alpha = keycol)) +
  geom_bar(stat = "identity", position = "identity")+
  scale_alpha_manual(values = c(0.05, 1))+
  
  facet_wrap(~vac.group,ncol=1)+
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
  scale_x_continuous(limits = c(0, 7500000),breaks = c(0,2500000, 5000000, 7500000), labels = label_comma(big.mark = ".", decimal.mark = ","))+
  
  labs(title = "Doelgroepen gevaccineerd",
       subtitle = "Absolute getallen",
       caption = paste("Source: github.com/YorickBleijenberg / Min VWS | Plot: @YorickB | ",Sys.Date()))+
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
    panel.grid.minor.y = element_blank())+
    
ggsave("data/91_vaccine_to_groups.png",width=9, height = 16)


#keycol <- "vac.group"
#valuecol <- "data"
#gathercols <- c("percentage", "hunderd.P")

#vaccine.groups.data.gather.perc <- gather(vaccine.groups.data, keycol, valuecol, gathercols)


#### group 100 percent plot ####





vaccine.groups.data.gather.perc$vac.group <- as.factor(vaccine.groups.data.gather.perc$vac.group)
levels(vaccine.groups.data.gather.perc$vac.group) <- c("Bewoners verpleeghuizen en mensen met een verstandelijke beperking",
                                                       "Medewerkers acute zorg",
                                                       "Medewerkers verpleeghuizen & gehandicaptenzorg & wijkverpleging",
                                                       "18-60 medische indicatie",
                                                       "Bewoners intramurale GGZ",
                                                       "Thuiswonenden van 60-75",
                                                       "Thuiswonenden 75+, en niet mobiele 60+",
                                                         "Medewerkers GGZ",
                                                         "Medewerkers zorg overig",
                                                       "De rest (muv kinderen)"
                                                       )


sodgfjsdljfh <- vaccine.groups.data$percentage

dat_text<- data.frame(
  label = c(sodgfjsdljfh),
  cyl   = c("Bewoners verpleeghuizen en mensen met een verstandelijke beperking",
            "Medewerkers acute zorg",
            "Medewerkers verpleeghuizen",
            "18-60 medichte indicatie",
            "Bewoners intramurale GGZ",
            "Thuiswonenden van 60-75",
            "Thuiswonenden 75+, en niet mobiele 60+",
            "Medewerkers GGZ",
            "Medewerkers zorg overig",
            "De rest (muv kinderen)"
  )
)







ggplot(vaccine.groups.data.gather.perc,aes(x = valuecol, y = "", fill = keycol,
                                                            color = keycol, 
                                                            alpha = keycol)) +
  geom_bar(stat = "identity", position = "identity")+
  
#  geom_label(vaccine.groups.data, aes(x= 50, y= "",  label= percentage))+
  
  #geom_text(data=dat_text, mapping = aes(x = 50, y =  "", label = label))+
  
  
  facet_wrap(~vac.group,ncol=1)+
  scale_alpha_manual(values = c(0.05, 1))+

  theme_classic()+
  xlab("")+ 
  ylab("")+
 #   scale_y_discrete ( labels = label_comma(big.mark = ".", decimal.mark = ","))+
   labs(title = "Doelgroepen gevaccineerd",
            subtitle = "Percentage v/d groep",
            caption = paste("Source: github.com/YorickBleijenberg / Min VWS | Plot: @YorickB | ",Sys.Date()))+
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
    panel.grid.minor.y = element_blank())+

ggsave("data/91_vaccine_to_groups_corr_size.png",width=9, height = 16)
  
