
### Preliminary analysis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

rm(list=ls())

#####################
## Load libraries
####################

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1]) ## install if needed on machine
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}

packs <- c('tm', 'stm', 
           'tidyr', 'quanteda')

loadPkg(c(packs))

########################
## Declare Data Paths
#########################

if(Sys.info()['user']=="Ergane"){ ## if on my own machine, look in Dropbox
    dataPathDesktop <- "~/Dropbox/WTO/rdatas/"
    print(paste0("The datapath is: ", dataPathDesktop))
}else{ ## else look up two directories
    dataPathDesktop <- "../../"
    print(paste0("The datapath is: ", dataPathDesktop))
}

#########################
## Load data for STM
#########################

## This has the WTO paragraph-level data,
## post-processing cleanup
## and wealth info

load(paste0(dataPathDesktop,"processedTextforSTM.Rdata"))

ls()

#################
#### Analysis
#### Search K at the 5-50 topic level
###########################

set.seed(80620)

### searchk

Ks=seq(from=10, to=50,by=10)

mod.tdsk <- searchK(documents=docs,
                       vocab=vocab,
                       K=Ks,
                    prevalence=~s(numdate)+
                    as.factor(income_level_iso3c),
                       data=meta,
                    seed=80620)

save(mod.tdsk,
     file=paste0(dataPathDesktop, "sK10to50.Rdata"))
 
