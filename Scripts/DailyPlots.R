
library(zoo)

# apply rolling mean over 10 data points
#Merged_data_7MA[, `:=`(rollMerged_data_7MA = frollmean(Merged_data_7MA$cases, n = 7, align = "center"), idx = .I)]
Merged_data_7MA$MACases <- rollmeanr(Merged_data_7MA$cases, 7, fill = 0)
Merged_data_7MA$MAhosp <- rollmeanr(Merged_data_7MA$hosp, 7, fill = 0)
Merged_data_7MA$MAdead <- rollmeanr(Merged_data_7MA$dead, 7, fill = 0)


#Gewenste dagen subsetten
Merged_data_short <- Merged_data_7MA[Merged_data_7MA$dateInTable>"2020-07-01"&Merged_data_7MA$dateInTable<=Sys.Date(),]




png("data/05_new_cases.png"
    , units = "px"
    , height = 1600
    , width = 1600
    , res = 300
)
par(bg="#F5F5F5") # The par() command's bg argument sets the background color for the entire plotting 
de.bar <- barplot(Merged_data_short$cases,
                  main = "Nieuw gemelde besmettingen",
                  names.arg = Merged_data_short$dateInTable,
                  col = "#96afde", border = NA
)
fig <- lines(x=de.bar, y = Merged_data_short$MACases, col = "#44546a",lwd = 3)
dev.off()

png("data/06_new_cases_log.png"
    , units = "px"
    , height = 1600
    , width = 1600
    , res = 300
)
par(bg="#F5F5F5") # The par() command's bg argument sets the background color for the entire plotting 
de.bar <- barplot(Merged_data_short$cases, main="Nieuw gemelde besmettingen, logaritmisch",
                  names.arg = Merged_data_short$dateInTable,
                  col = "#96afde", border = NA, log="y"
)
fig <- lines(x=de.bar, y = Merged_data_short$MACases, col = "#44546a",lwd = 3)
dev.off()


png("data/09_new_hosp.png"
    , units = "px"
    , height = 1600
    , width = 1600
    , res = 300
)
par(bg="#F5F5F5") # The par() command's bg argument sets the background color for the entire plotting 
de.bar <- barplot(Merged_data_short$hosp, main="Nieuw gemelde opnames", col = "#f4b183",border = NA)
fig <- lines(x=de.bar, y = Merged_data_short$MAhosp, col = "#c55a11",lwd = 3)
dev.off()

png("data/13_new_deceased.png"
    , units = "px"
    , height = 1600
    , width = 1600
    , res = 300
)
par(bg="#F5F5F5") # The par() command's bg argument sets the background color for the entire plotting 
de.bar <- barplot(Merged_data_short$dead, main="Nieuw gemelde overledenen", col = "#fab0b0", border = NA)
fig <- lines(x=de.bar, y = Merged_data_short$MAdead, col = "#ff0505",lwd = 3)
dev.off()