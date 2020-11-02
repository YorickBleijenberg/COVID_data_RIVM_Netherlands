
###   Make GR line smooth
###   Cumulatieve getallen

##  Connect to online:
## https://happygitwithr.com/

## compare R prediction
##






#### check selected dates ####

library(tidyverse)
library(zoo)
library(RcppRoll)
require(data.table)


rm(list=ls())

dirname <- paste("data/",Sys.Date(),sep = "")
dir.create(dirname)

source("C:\\Rdir\\Rscripts\\geom_stream.R")

source("C:\\Rdir\\Rscripts\\00_lcps_data_graph.R")


#### Start  ####

source("C:\\Rdir\\Rscripts\\02_RIVM_data_importer_and_archiver.R")


  source("C:\\Rdir\\Rscripts\\03A_TwitterAuthentication.r")
##  source("C:\\Rdir\\Rscripts\\03test_TwitterAuthentication.r")


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

source("C:\\Rdir\\Rscripts\\08_caseType_diff.R")
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

source("C:\\Rdir\\Rscripts\\12_cities.R")

source("C:\\Rdir\\Rscripts\\14_provincies.R")

source("C:\\Rdir\\Rscripts\\17_Herstvakantie-effect.R")



#### check correct last tweet ####

## source("C:\\Rdir\\Rscripts\\10_EN_TwitterTread.R")


##source("C:\\Rdir\\Rscripts\\10_TwitterTread.R")

