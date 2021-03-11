require(tabulizer)
require(tidyverse)
require(lubridate)
require(scales) 

####
####  parse code by mzelst  ###
####

report<-"C:\\Rdir\\rivm-week\\2021-09-COVID-19_WebSite_rapport_wekelijks_20210309_1259.pdf"
weeknumber <-09


report<-"https://www.rivm.nl/sites/default/files/2021-02/COVID-19_WebSite_rapport_wekelijks_20210202_1259_final.pdf"
weeknumber <- isoweek(Sys.Date())-1

#### tests per agegroup GGD ####

area.table.ggd.age.tests <- locate_areas(report,
                                          pages=c(37))

ggd_age_tests <- extract_tables(report,
                           output = "data.frame",
                           pages = c(37),
                           area = area.table.ggd.age.tests,
                           guess=FALSE)

ggd_age_tests <- do.call(rbind,ggd_age_tests)


colnames(ggd_age_tests) <- c("agegroup","Totaal.aantal.testen","Aantal.positief","percentage.positief","aantal.testen.week","Aantal.positief.week","percentage.positief.week")
#colnames(ggd_age_tests) <- c("agegroup","Totaal.aantal.testen.man","Aantal.positief.man","percentage.positief.man","aantal.testen.week.vrouw","Aantal.positief.week.vrouw","percentage.positief.week.vrouw")


ggd_age_tests <- ggd_age_tests[-c(1),]

ggdTestFile <- paste0("data-dashboards/2021-",weeknumber,"-ggd_tests_per_agegroup.csv")
write.csv(ggd_age_tests,file = ggdTestFile, row.names = F)



#### tests per target group GGD ####

area.table.target.tests <- locate_areas(report,
                                         pages=c(39))

ggd_target_tests <- extract_tables(report,
                                output = "data.frame",
                                pages = c(39),
                                area = area.table.target.tests,
                                guess=FALSE)

ggd_target_tests <- do.call(rbind,ggd_target_tests)


colnames(ggd_target_tests) <- c("group","Totaal.aantal.testen","Aantal.positief","percentage.positief","aantal.testen.week","Aantal.positief.week","percentage.positief.week")

ggd_target_tests <- ggd_target_tests[-c(1),]

ggdTestFile <- paste0("data-dashboards/2021-",weeknumber,  "-ggd_tests_target-group.csv")
write.csv(ggd_target_tests,file = ggdTestFile, row.names = F)




