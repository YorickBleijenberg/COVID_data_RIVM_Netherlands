##
###
##



start.vaccination.nl = as.Date(c("2021-01-06"))
days.vaccination.in.nl = as.numeric(Sys.Date() - start.vaccination.nl+1)

vaccinated.people.total = 532717
vaccinated.second = 40000+15000
vaccinated.first = vaccinated.people.total-vaccinated.second

vac.perc <-  round((vaccinated.first/17474693*100), digits =4)
vac.perc <- format(vac.perc, scientific=F)
vac.perc.18 <-  round((vaccinated.first/14070340*100), digits =4)
vac.perc.18 <- format(vac.perc.18, scientific=F)

vac.perc.second <-  round((vaccinated.second/17474693*100), digits =4)
vac.perc.second <- format(vac.perc.second, scientific=F)
vac.perc.18.second <-  round((vaccinated.second/14070340*100), digits =4)
vac.perc.18.second <- format(vac.perc.18.second, scientific=F)

freezer = 1042948
spillage <- as.integer(vaccinated.people.total/95)*5
in.freezer <- freezer-vaccinated.people.total-spillage

days.vaccination.in.nl
in.freezer
spillage
vaccinated.people.total
vac.perc.18
vac.perc
vac.perc.18.second
vac.perc.second

