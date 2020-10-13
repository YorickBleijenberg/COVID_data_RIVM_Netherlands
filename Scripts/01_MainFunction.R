
rm(list=ls())

###   
###   Make GR line smooth
###   other potision for caseType 8b
###   "Bron: RIVM | Plot: @YorickB | ",Sys.Date()
###   Cities
###     Cumulatieve getallen




#scale_x_date(date_breaks = "1 week", 
  #           date_labels= format("%d-%b"),
   #          limits = as.Date(c("2020-07-01", Sys.Date()+5)))+

### check selected dates

source("C:\\Rdir\\Rscripts\\02_RIVM_data_importer_and_archiver.R")

source("C:\\Rdir\\Rscripts\\03A_TwitterAuthentication.r")

#  source("C:\\Rdir\\Rscripts\\03test_TwitterAuthentication.r")

source("C:\\Rdir\\Rscripts\\04_DataPrep.R")

print(Working_Set)


source("C:\\Rdir\\Rscripts\\05_DailyPlots.R")
    ## 05_new_cases.png
    ## 06_new_cases_log.png
    ## 09_new_hosp.png
    ## 13_new_deceased.png

source("C:\\Rdir\\Rscripts\\06_WoW_DoD_plots.R")
    ## 08_new_cases_WoW.png
    ## 07_new_cases_DoD.png

source("C:\\Rdir\\Rscripts\\07_Age-heatmap_barchart.R")
    ## 02_leeftijd_heatmap.png
    ## 01_leeftijd_barchart.png
    ## 03_leeftijd_relatief.png

source("C:\\Rdir\\Rscripts\\07A_Age-heatmap_barchart-hosp.R")
    ## 02_leeftijd_heatmap-hosp.png
    ## 02_leeftijd_heatmap-dead.png

source("C:\\Rdir\\Rscripts\\08_caseType.R")
    ## 07_cases_diff.png

source("C:\\Rdir\\Rscripts\\08b_caseType.R")
    ## 07_cases_type1.png

source("C:\\Rdir\\Rscripts\\09_WeekOfDeath-plot.R")
    ## 15_dead_diff

source("C:\\Rdir\\Rscripts\\09A_growth.R")
    ## 05_growth_cases.png
    ## 05_growth_hosp.png
    ## 05_growth_dead.png
    ## scale x-axes (date)

source("C:\\Rdir\\Rscripts\\10_TwitterTread.R")




###
###
###

tweet.cases.tweet
"data/05_new_cases.png", 
"data/05_growth_cases.png", 
"data/07_cases_type1.png", 
"data/08_new_cases_WoW.png"

tweet.cases.diff.tweet
"data/07_cases_diff.png",
"data/07_cases_type1.png"

tweet.hosp.tweet
"data/09_new_hosp.png",
"data/02_leeftijd_heatmap-hosp.png"

tweet.dead.tweet
"data/02_leeftijd_heatmap-dead.png",
"data/13_new_deceased.png", 
"data/15_dead_diff.png"

tweet.age.tweet
"data/01_leeftijd_barchart.png",
"data/02_leeftijd_heatmap.png", 
"data/03_leeftijd_relatief.png"

tweet.data.tweet
 "dagen tot"

 
 
 
 
 
 labs(title = paste("Current week over week growth of new cases: ", gf_c_last, "%"),
      subtitle = "based on the 7 day moving average",
      caption = paste("Source: RIVM | Plot: @YorickB | ",Sys.Date()))+
 
     plot.title = element_text(hjust = 0.5,size = 30,face = "bold"),
 plot.subtitle =  element_text(hjust=0.5,size = 20,color = "black", face = "italic"),
     
 
 
 labs(title = "Cases COVID-19",
      subtitle = "Number of cases per 100.000, within each agegroup. Week 40 and 41 will still rise.",fill=NULL,
      caption = paste("Source: RIVM / CBS | Plot: @YorickB | ",Sys.Date()))+     
     
     
     
     
     
 
     labs(title = "New cases",
          subtitle = "with 7 day moving average",
          caption = paste("Source: RIVM / CBS | Plot: @YorickB | ",Sys.Date()))+


     
     
     
