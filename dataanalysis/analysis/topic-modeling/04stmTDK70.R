### Preliminary analysis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

## After running search K in 50-100 range,
## K=60 and K=90 stood out as the two lowest residuals
## first analysis with K=60 for ease of human interpretation


rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 'pdftools',
           'tidyr', 'quanteda', "wbstats")

loadPkg(packs)


########################
## Declare Data Paths
#########################


if(Sys.info()['user']=="Ergane"){ ## if on my own machine look in Dropbox
    dataPathDesktop <- "~/Dropbox/WTO/rdatas/"
    print(paste0("The datapath is: ", dataPathDesktop))
}else{ ## else look in ~/WTO/
    dataPathDesktop <- "../../"
    print(paste0("The datapath is: ", dataPathDesktop))
}

########################
## Load Centrally-processed data
########################

load(paste0(dataPathDesktop,"processedTextforSTM.Rdata"))
#########################
##### Analysis
##########################
############################
#### K=70
###########################

set.seed(61920)


mod.out.70 <- stm(documents=docs,
               vocab=vocab,
               data=meta,
               K=70, ## 60 suggested by searchK
                  prevalence= ~ s(numdate) +
                  as.factor(income_level_iso3c),
               seed=61920)

 

prep.70 <- estimateEffect(c(1:70)~ s(numdate) +
                          as.factor(income_level_iso3c),
                       mod.out.70,
                       metadata=meta,
                       documents=docs,
                       uncertainty=c("Global"))

save.image(file=paste0(dataPathDesktop,
               "tradDevPara_70.RData"))
