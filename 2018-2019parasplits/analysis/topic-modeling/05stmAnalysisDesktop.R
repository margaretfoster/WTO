##library(stminsights)

##devtools::install_github('rstudio/shiny')
##devtools::install_github('cschwem2er/stminsights')
library(stm)

dataPathDesktop <- "~/Dropbox/WTO/rdatas/"


load(paste0(dataPathDesktop, "tradDevPara75_60.Rdata"))

ls()

summary(mod.out.60)

