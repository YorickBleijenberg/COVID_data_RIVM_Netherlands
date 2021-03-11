
  
new.date <- Sys.Date()
old.date <- new.date-1


#######################################
File_date_46 <- paste0("data/",old.date,"/",old.date,"_disabled_daily_dif.csv")
diff.file  <- read.csv(File_date_46 ,sep=";", check.names = FALSE)
diff.file$date <- as.Date(diff.file$date)


old.disabled.file <- paste0("https://github.com/mzelst/covid-19/raw/master/data-rivm/disabled-people-per-day/rivm_daily_", old.date, ".csv.gz", sep = "")
old.casus <- fread(old.disabled.file)
old.casus$Date_statistics <- as.Date(old.casus$Date_statistics)

new.disabled.file <- paste0("https://github.com/mzelst/covid-19/raw/master/data-rivm/disabled-people-per-day/rivm_daily_", new.date, ".csv.gz", sep = "")
new.casus <- fread(new.disabled.file)
new.casus$Date_statistics <- as.Date(new.casus$Date_statistics)

#### calulate differenc between yesterday and today
old.total.cases.disabled     <- sum(old.casus$Total_cases_reported)
old.total.deceased.disabled  <- sum(old.casus$Total_deceased_reported)
new.total.cases.disabled     <- sum(new.casus$Total_cases_reported)
new.total.deceased.disabled  <- sum(new.casus$Total_deceased_reported)
diff.cases.disabled     <-  new.total.cases.disabled    - old.total.cases.disabled
diff.deceased.disabled  <-  new.total.deceased.disabled - old.total.deceased.disabled

### create new DF. ####
new.line.two <- c(as.character(new.date), diff.cases.disabled,diff.deceased.disabled)
cassus.age.merge.fin <- rbind(diff.file, new.line.two)

cassus.age.merge.fin$new_cases  <- as.integer(cassus.age.merge.fin$new_cases)
cassus.age.merge.fin$new_dead   <- as.integer(cassus.age.merge.fin$new_dead)

##### write file
File_date_99 <- paste0("data/",new.date,"/",new.date,"_disabled_daily_dif.csv")
write.csv2(cassus.age.merge.fin, File_date_99, row.names=FALSE)
File_date_99 <- paste0("data//casus_vac_effect/disabled_daily_dif.csv")
write.csv(cassus.age.merge.fin, File_date_99, row.names=FALSE)




disabled.play <- cassus.age.merge.fin



### get 7 day MA
disabled.play$MA.disabled.cases.change   <- rollmeanr(disabled.play$new_cases   , 7, fill = 0)


### find MAX
maxDisableCases <-  max(disabled.play$MA.disabled.cases.change, na.rm = TRUE)  # 81.857

### normalize
disabled.play$MA.disabled.cases.change.relative  <- disabled.play$MA.disabled.cases.change /maxDisableCases

disabled.play <- disabled.play[disabled.play$date >"2020-12-17",]






#### plot


ggplot(disabled.play)+
  
#  geom_ribbon(data  = casus.age.dif.play.short, 
#              aes(x = date  , ymin=MA.old90p.Change.relative, ymax=MA.young.Change.relative), 
#              fill = "blue" , alpha = 0.25) +
  
  geom_line(  aes(x= date, MA.disabled.cases.change.relative),  lwd=4, color="#F5F5F5")+
  geom_line(  aes(x= date, MA.disabled.cases.change.relative),  lwd=3, color="#B8DE29FF")+
 # geom_line(  aes(x= date, MA.middle.Change.relative),  lwd=4, color="#F5F5F5")+
 # geom_line(  aes(x= date, MA.middle.Change.relative),  lwd=3, color="#55C667FF")+
 # geom_line(  aes(x= date, MA.old.Change.relative),    lwd=4, color="#F5F5F5")+
 # geom_line(  aes(x= date, MA.old.Change.relative),    lwd=3, color="#1F968BFF")+
 # geom_line(  aes(x= date, MA.old80.Change.relative),    lwd=4, color="#F5F5F5")+
 # geom_line(  aes(x= date, MA.old80.Change.relative),    lwd=3, color="#404788FF")+
 # geom_line(  aes(x= date, MA.old90p.Change.relative),    lwd=4, color="#F5F5F5")+
 # geom_line(  aes(x= date, MA.old90p.Change.relative),    lwd=3, color="#440154FF")+
  
 # annotate("text", x = as.Date(dag.label), y = 0.43, label = "  50 min",  size=5, face = "bold", color = "#B8DE29FF")+
 # annotate("text", x = as.Date(dag.label), y = 0.36, label = " 50-69" ,  size=5, face = "bold", color = "#55C667FF")+
 # annotate("text", x = as.Date(dag.label), y = 0.33, label = " 70-79" ,  size=5, face = "italic", color = "#1F968BFF")+
 # annotate("text", x = as.Date(dag.label), y = 0.25, label = " 80-89" ,  size=5, face = "bold", color = "#404788FF")+
 # annotate("text", x = as.Date(dag.label), y = 0.17, label = "90+"   ,  size=5, face = "bold", color = "#440154FF")+
  
  scale_y_continuous(limits = c(0, 1),labels = percent)+
  scale_x_date(date_breaks  = "1 months",date_labels= format("%b"),
               limits  = as.Date(c("2020-10-01", NA)))+
  
  theme_classic()+
  
  xlab("")+
  ylab("")+
  
  geom_vline(xintercept = as.Date("2021-02-21"), linetype = "dotted") + 
  annotate("text", x  = as.Date("2021-02-22"), y = 0.7, label = "57% \nvan de 85+ers heeft \neen eerste prik ontvangen \nen 61% van de 90+ers \n(thuiswonend)", size=3, angle=0, vjust=-0.4, hjust = 0, color = "black")+
  
  labs(title     = "Besmettingen",
      # subtitle = subtitle.label,
       caption   = paste("Bron: RIVM | Plot: @YorickB  | ",Sys.Date()))+
  
  theme(#legend.position = "none",   # no legend
    legend.title       = element_blank(),  ## legend title
    legend.position    = "top",
    legend.direction   = "vertical",
    legend.background  = element_rect(fill="#f5f5f5", size=0.5, linetype="solid"))+
  
  theme(
    plot.background    = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background   = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title         = element_text(hjust = 0.5 ,size = 40 ,face = "bold"),
    plot.subtitle      = element_text(hjust=0.5   ,size = 15 ,color = "black", face = "italic"),
    axis.text          = element_text(size=14,color = "black",face = "bold"),
    axis.ticks         = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length  = unit(0.5, "cm"),
    axis.line          = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))+
  
  ggsave("data/79_disabled_case.png",width=16, height = 9)












#dt.old = 2021-03-01.csv.gz")
#dt.new = fread("https://github.com/mzelst/covid-19/raw/master/data-rivm/disabled-people-per-day/rivm_daily_2021-03-07.csv.gz")
#write.csv2(dt.old, file = "data/disabled.old.v.csv",row.names = F)
#write.csv(dt.new, file = "data/disabled.new.csv",row.names = F)

#n= 87

#new.date <- Sys.Date()-n
#old.date <- new.date-1

#old.disabled.file <- paste0("https://github.com/mzelst/covid-19/raw/master/data-rivm/disabled-people-per-day/rivm_daily_", old.date, ".csv", sep = "")
#old.casus <- fread(old.disabled.file)
#old.casus$Date_statistics <- as.Date(old.casus$Date_statistics)

#new.disabled.file <- paste0("https://github.com/mzelst/covid-19/raw/master/data-rivm/disabled-people-per-day/rivm_daily_", new.date, ".csv", sep = "")
#new.casus <- fread(new.disabled.file)
#new.casus$Date_statistics <- as.Date(new.casus$Date_statistics)

#### calulate differenc between yesterday and today
#old.total.cases.disabled     <- sum(old.casus$Total_cases_reported)
#old.total.deceased.disabled  <- sum(old.casus$Total_deceased_reported)
#new.total.cases.disabled     <- sum(new.casus$Total_cases_reported)
#new.total.deceased.disabled  <- sum(new.casus$Total_deceased_reported)
#diff.cases.disabled     <-  new.total.cases.disabled    - old.total.cases.disabled
#diff.deceased.disabled  <-  new.total.deceased.disabled - old.total.deceased.disabled

### create new DF. ####
#new.line <- setNames(data.frame(matrix(ncol = 3, nrow = 1)), c("date", "new_cases", "new_dead"))
#new.line.two <- c(as.character(new.date), diff.cases.disabled,diff.deceased.disabled)
#cassus.age.merge.fin <- rbind(new.line, new.line.two)
#cassus.age.merge.fin = cassus.age.merge.fin[-1,]


#n= 58
#n=1

#while (n > 55){
#n <- n-1

#}
#cassus.age.spread.base  <- read.csv("C:\\Rdir\\data-contstant\\casus_daily_age_dif.csv" ,sep=";", check.names = FALSE)
