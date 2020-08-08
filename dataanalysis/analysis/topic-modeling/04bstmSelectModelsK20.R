#########
### Model select K20
#########

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 'pdftools',
           'tidyr', 'quanteda', "wbstats")

loadPkg(packs)


#########################
## Declare Data Paths
#########################

if(Sys.info()['user']=="Ergane"){ ## if on my own machine look in Dropbox
    dataPathDesktop <- "~/Dropbox/WTO/rdatas/"
    print(paste0("The datapath is: ", dataPathDesktop))
}else{ ## else look in ~/WTO/
    dataPathDesktop <- "../../"
    print(paste0("The datapath is: ", dataPathDesktop))
}

#######################
## Load Processed Data
#######################

load(paste0(dataPathDesktop,"processedTextforSTM.Rdata"))

#############################
##### Analysis
#############################
#############################
#### Model Search K=20
#############################

set.seed(61920)


mod.select.20 <- stm::selectModel(documents=docs,
                               vocab=vocab,
                               data=meta,
                               K=20, ## K20
                               prevalence= ~ s(numdate) +
                               as.factor(income_level_iso3c),
                               seed=61920)


save(mod.select.20,
     file=paste0(dataPathDesktop,
               "tradDevModSelect_20.RData"))
