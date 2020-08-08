##### Since we have no clear diagnostics
##### on topic number to choose
#### seeing what the built-in "inductive"
#### discovery algorithm chooses 

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}

packs <- c('tm', 'stm')

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

###############################
### Load centrally-processed data
###############################

load(paste0(dataPathDesktop,"processedTextforSTM.Rdata"))

ls()

#################
##### Analysis
##################

############################
#### Inductive K discovery
###########################

set.seed(61920)


mod.out.Ind <- stm(documents=docs,
               vocab=vocab,
                   data=meta,
                   init="Spectral",
                   K=0, ## What does STM come up with?
                   prevalence= ~ s(numdate) +
                   as.factor(income_level_iso3c),
                   seed=61920)

save(mod.out.Ind,
     file=paste0(dataPathDesktop,
         "modoutInductiveK.Rdata"))

############################
## Optional: Run N more times
## To check for consistency
############################
## Run a few more and see what I get:

## R=10
## for(r in 1:R){
    
##     mod.out.Ind <- stm(documents=docs,
##                        vocab=vocab,
##                        data=meta,
##                        init="Spectral",
##                        K=0, ## What does STM come up with?
##                        prevalence= ~ s(numdate) +
##                        as.factor(income_level_iso3c),
##                        seed=61920)

## save(mod.out.Ind,
##      file=paste0(dataPathDesktop,
##          "modoutInductiveK",r,".Rdata"))
## }  

