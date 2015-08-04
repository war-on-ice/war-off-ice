## global file for playerseason server
## Last edited: August 31, 2014

library(shiny)
library(warbase)

#source("../warbase/R/urls.R")
#source("../warbase/R/variables.R")

load ("../common-data/woi-common.RData")
season.breaks <- read.csv("../source-data/seasonbreaks.csv")
season.breaks[,1] <- as.Date(season.breaks[,1]); season.breaks[,2] <- as.Date(season.breaks[,2])
team.options <- c("All", sort(teams))

#yeardiff <- function (d1, d2) {
    ## d1 = "2015-06-15"; d2 = "1991-02-17"
#    as.numeric(substr(d1,1,4)) - as.numeric(substr(d2,1,4)) - 1*(substr(d1,5,10) < substr(d2,5,10))
#}

ageAtEnd <- function (year1, dob) {
    yeardiff(paste0(year1, "-06-30"), dob)
}


contracts <- {
    load("../common-data/contracts-complete.RData")
    contracts.complete %>% rename (AAV = aAAV)## assemble.contract.data ()
}
##print(head(contracts,2))

##contracts <- get(load (url("http://biscuit.war-on-ice.com/common-data/contracts-complete.RData")))
