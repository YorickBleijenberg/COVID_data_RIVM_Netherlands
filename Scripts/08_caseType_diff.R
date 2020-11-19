#library(tidyverse)
#library(tidyr)
require(zoo)
require(data.table)

#library(RcppRoll)

###########   How to plot factors in a specified order in ggplot


temp = last(list.files(path = "rivm-data",pattern="*.csv", full.names = T),2) ## Pull names of all available datafiles
myfiles = lapply(temp, read.csv, sep=";")


df <- map_dfr(myfiles, ~{
  .x
})

df$value <- 1
df$Date_file <- as.Date(df$Date_file)

df_date_long <- aggregate(df$value, by = list(Type_Datum = df$Date_statistics_type, Datum = df$Date_statistics, Dag = df$Date_file), FUN = sum)

df_date_wide <- spread(df_date_long, key = Dag, value = x)


df_date_wide$Verschil <- df_date_wide[,ncol(df_date_wide)] - df_date_wide[,ncol(df_date_wide)-1]
ncol(df_date_wide)-1

df_date_wide <- df_date_wide[,c("Datum","Verschil","Type_Datum")]

df.final <- spread(df_date_wide, key = Type_Datum, value = Verschil, fill = 0)
colnames(df.final) <- c("Datum","DON_diff","DOO_diff","DPL_diff")
df.final$Datum <- as.Date(df.final$Datum)

temp = last(list.files(path = "rivm-data",pattern="*.csv", full.names = T)) ## Pull names of all available datafiles
dat.today <- read.csv(temp, sep=";")

date_type.df <- as.data.frame(table(dat.today$Date_statistics, dat.today$Date_statistics_type))
date_type_wide <- spread(date_type.df, key = Var2, value = Freq)
date_type_wide$Datum <- as.Date(date_type_wide$Var1)
date_type_wide <- date_type_wide[,c("DON","DOO","DPL","Datum")]

dat_wide <- merge(date_type_wide, df.final, by = "Datum")


aantal <- nrow(dat_wide)
tooReplace1 <- tail(dat_wide$DON,n=1)
tooReplace2 <- tail(dat_wide$DOO,n=1)
tooReplace3 <- tail(dat_wide$DPL,n=1)

dat_wide[aantal,5] <- tooReplace1
dat_wide[aantal,6] <- tooReplace2
dat_wide[aantal,7] <- tooReplace3

dat_wide <- within(dat_wide, rm(DOO, DON, DPL))



key <- "Datum"
value <- "type"
gathercols <- c("DPL_diff","DON_diff","DOO_diff")

dat_wide <- gather(dat_wide, key, value, all_of(gathercols))
  
two.weeks.ago <-as.Date(Sys.Date()-15)
dat_wide <- dat_wide[dat_wide$Datum>two.weeks.ago,]

ggplot(dat_wide, aes(x=Datum, y=value, fill = factor(key, levels=c("DOO_diff","DPL_diff","DON_diff")), width=.7)) +
  geom_col(position = position_dodge(width = 0.65))+
  
  theme_classic()+
  
  xlab("")+ 
  ylab("")+
  
  scale_x_date(date_breaks = "1 day", 
               date_labels= format("%d-%b"),
               limits = as.Date(c(Sys.Date()-15, Sys.Date()+1)))+
  
  scale_fill_manual(values=c("#548235", "#2f5597", "#c55a11"), labels=c( "Eerste ziektedag (nieuw/correctie)",
                                                                         "Positieve labuitslag (nieuw/correctie)",
                                                                         "Melding aan GGD (nieuw/correctie)"
                                                                         ))+
  
  labs(title = "Besmette personen: toegevoegd / gecorrigeerd",
       caption = paste("Bron: RIVM | Plot: @YorickB | ",Sys.Date()))+

  theme(legend.position = c(0.2, 0.9),
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"),
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    axis.text.x = element_text(face="bold", color="black", size=12),
    axis.text.y = element_text(face="bold", color="black", size=14),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))


ggsave("data/07_cases_diff.png",width=16, height = 9)  

  
    

ggplot(dat_wide, aes(x=Datum, y=value, fill = factor(key, levels=c("DOO_diff","DPL_diff","DON_diff")), width=.7)) +
  geom_col(position = position_dodge(width = 0.65))+
  
  theme_classic()+
  xlab("")+ 
  ylab("")+
  
  scale_x_date(date_breaks = "1 day", 
               date_labels= format("%d-%b"),
               limits = as.Date(c(Sys.Date()-15, Sys.Date()+1)))+
  
  scale_fill_manual(values=c("#548235", "#2f5597", "#c55a11"), labels=c( "First day with symptoms (new/correction)",
                                                                         "Positive lab result (new/correction)",
                                                                         "Notification to GGD (new/correction)"
                                                                         ))+
  
  labs(title = "New cases: added / corrected today",
       caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+
  
  theme(legend.position = c(0.2, 0.9),
        legend.background = element_rect(fill="#F5F5F5",size=0.8,linetype="solid",colour ="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10, face="bold"))+
  
  theme(
    plot.background = element_rect(fill = "#F5F5F5"),
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5,size = 20,color = "black", face = "italic"),
    axis.text.x = element_text(face="bold", color="black", size=12),
    axis.text.y = element_text(face="bold", color="black", size=14),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.y = element_line(colour= "lightgray", linetype = "dashed"))
  
ggsave("data/07_EN_cases_diff.png",width=16, height = 9)  

