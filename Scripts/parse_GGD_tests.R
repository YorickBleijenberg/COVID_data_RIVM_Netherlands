require(tabulizer)
require(tidyverse)
require(lubridate)
require(scales) 

####
####  parse code by mzelst  ###
####


weeknumber <- isoweek(Sys.Date())-1


report <- "https://www.rivm.nl/sites/default/files/2020-12/COVID-19_WebSite_rapport_wekelijks_20201222_1259.pdf"



#### tests GGD ####


area.table.ggd.total <- locate_areas(report,
                                          pages=c(30))

ggd_tests <- extract_tables(report,
                           output = "data.frame",
                           pages = c(30),
                           area = area.table.ggd.total,
                           guess=FALSE)

ggd_tests <- do.call(rbind,ggd_tests)

colnames(ggd_tests) <- c("Week","Totaal.aantal.testen","Aantal.positief","percentage.positief")

ggdTestFile <- paste0("data-dashboards/ggd_tests_", weeknumber, ".csv")
write.csv(ggd_tests,file = ggdTestFile, row.names = F)


ggd_tests_small <- ggd_tests

### remove header
#ggd_tests_small <- ggd_tests[-c(1), ]
#ggd_tests_small<- ggd_tests_small[-seq(nrow(ggd_tests_small),nrow(ggd_tests_small)),]

ggd_tests_small$Week <- c(24:weeknumber)

#  ggd_tests_small[nrow(ggd_tests_small),1] <- weeknumber

ggd_tests_small$Week <- as.integer(ggd_tests_small$Week)
ggd_tests_small$Totaal.aantal.testen <- as.integer(ggd_tests_small$Totaal.aantal.testen)
ggd_tests_small$Week <- as.integer(ggd_tests_small$Week)

ggd_tests_small$fact <- (10000 * ggd_tests_small$percentage.positief)


#### plot GGD tests ####

text_sub <- paste("De gegevens van week", weeknumber, "zijn nog niet volledig")

n_tests <- ggd_tests_small[nrow(ggd_tests_small),2]
n_pos  <- ggd_tests_small[nrow(ggd_tests_small),4]

n_tests <- format(n_tests, big.mark="." ,decimal.mark=",")


text_title <- paste("Aantal testen met uitslag van de GGD'en vorige week:",
                    n_tests,
                    "\nPercentage positief:",
                    n_pos,
                    "%"
                    )

ggplot(data = ggd_tests_small,)+  
  geom_bar(stat='identity', mapping = aes(x = Week, y = Totaal.aantal.testen), fill = "#ED7D31")+
  geom_line(mapping = aes(x = Week, y = fact), colour = "#FFFFFF", size = 5 )+
  geom_line(mapping = aes(x = Week, y = fact), colour = "#4472C4", size = 4 )+
  
  geom_point(mapping = aes(x = Week, y = fact), colour = "#FFFFFF",size = 7) +
  geom_point(mapping = aes(x = Week, y = fact), colour = "#4472C4",size = 6,alpha = 0.8) +
  
  
  scale_y_continuous(limits = c(0, 550000), labels = label_number(big.mark = ".", decimal.mark = ","),
                     sec.axis = sec_axis(~ . / 10000))+
  
  scale_x_continuous("", breaks = ggd_tests_small$Week)+
  
  theme_classic()+
  
labs(x = "",
     y = "",
      title = text_title,
       subtitle = paste("tabel 13."),
       caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+

theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
       panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
       legend.position = "none",   # no legend
       plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
       plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
       axis.text = element_text(size=14,color = "black",face = "bold"),
       axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
       axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
       axis.ticks.length = unit(0.5, "cm"),
       axis.line = element_line(colour = "#F5F5F5"),
       panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
       
       axis.text.y.left = element_text(color = "#ED7D31"),
       axis.text.y.right = element_text(color = "#4472C4")
       )+
  
  geom_text( aes( x=41.5, y=550000, label=text_sub),
            color="#ED7D31", 
            size=7 , angle=0, fontface="bold")+
  
  
  annotate("curve", x = 48.5, xend =51, 
           y = 550000, yend = 500000, curvature = -0.2,
           colour = "black", size=2, alpha=0.7, arrow =arrow(type = "open",length = unit(2,"mm")))
  

ggsave("data/22_tests_ggden.png",width=16, height = 9)




#### tests labs ####


area.table.labs.total <- locate_areas(report,
                                     pages=c(43))

labs_tests <- extract_tables(report,
                            output = "data.frame",
                            pages = c(43),
                            area = area.table.labs.total,
                            guess=FALSE)

labs_tests <- do.call(rbind,labs_tests)

colnames(labs_tests) <- c("Week","Aantal_labs","Tests","Aantal_positief","Perc_positief")

#labs_tests <- labs_tests[c(2:(nrow(labs_tests))),]
labs_tests$Week <- c(12:weeknumber)


LabTestFile <- paste0("data-dashboards/Lab_tests_", weeknumber, ".csv")
write.csv(labs_tests,file = LabTestFile, row.names = F)



#labs_tests$Week <- as.integer(labs_tests$Week)
#labs_tests$Tests <- as.integer(labs_tests$Tests)
#labs_tests$Week <- as.integer(labs_tests$Week)

labs_tests$fact <- (10000 * labs_tests$Perc_positief)


#### plot labs tests ####

text_sub2 <- paste("De gegevens van week", weeknumber, "zijn nog niet volledig")

n2_tests <- labs_tests[nrow(labs_tests),3]
n2_pos  <- labs_tests[nrow(labs_tests),5]


n2_tests <- format(n2_tests, big.mark="." ,decimal.mark=",")

text_title2 <- paste("Aantal testen met uitslag van de labs vorige week:",
                    n2_tests,
                    "\nPercentage positief:",
                    n2_pos,
                    "%"
)

ggplot(data = labs_tests)+  
  geom_bar(stat='identity', mapping = aes(x = Week, y = Tests), fill = "#F4B183")+
  geom_line(mapping = aes(x = Week, y = fact), colour = "#FFFFFF", size = 5 )+
  geom_line(mapping = aes(x = Week, y = fact), colour = "#4472C4", size = 4 )+
  
  geom_point(mapping = aes(x = Week, y = fact), colour = "#FFFFFF",size = 7) +
  geom_point(mapping = aes(x = Week, y = fact), colour = "#4472C4",size = 6,alpha = 0.8) +
  
  
  scale_y_continuous(limits = c(0, 300000), labels = label_number(big.mark = ".", decimal.mark = ","),
                     sec.axis = sec_axis(~ . / 10000))+
  
  scale_x_continuous("", breaks = labs_tests$Week)+
  
  theme_classic()+
  
  labs(x = "",
       y = "",
       title = text_title2,
       subtitle = paste("tabel 17."),
       caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme( plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
         panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
         legend.position = "none",   # no legend
         plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
         plot.subtitle =  element_text(hjust=0.5 ,size = 15,color = "black", face = "italic"),
         axis.text = element_text(size=14,color = "black",face = "bold"),
         axis.text.y = element_text(face="bold", color="black", size=14),  #, angle=45),
         axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
         axis.ticks.length = unit(0.5, "cm"),
         axis.line = element_line(colour = "#F5F5F5"),
         panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"),
         
         axis.text.y.left = element_text(color = "#ED7D31"),
         axis.text.y.right = element_text(color = "#4472C4"))+
  
  geom_text( aes( x=39.5, y=275000, label=text_sub),
             color="#ED7D31", 
             size=7 , angle=0, fontface="bold")+
  
  annotate("curve", x = 49.5, xend =51, 
           y = 275000, yend = 250000, curvature = -0.2,
           colour = "black", size=2, alpha=0.7, arrow =arrow(type = "open",length = unit(2,"mm")))

ggsave("data/22_tests_labs.png",width=16, height = 9)

