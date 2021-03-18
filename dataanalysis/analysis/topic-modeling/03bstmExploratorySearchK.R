
### Preliminary analysis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 
           'tidyr', 'quanteda')

loadPkg(c(packs))

########################
## Declare Data Paths
#########################


if(Sys.info()['user']=="Ergane"){ ## if on my own machine look in Dropbox
    dataPathDesktop <- "~/Dropbox/WTO/rdatas/"
    print(paste0("The datapath is: ", dataPathDesktop))
}else{ ## else look in ~/WTO/
    dataPathDesktop <- ""
    print(paste0("The datapath is: ", dataPathDesktop))
}

## This has the WTO paragraph-level data,
## post-processing cleanup
## and wealth info

load(paste0(dataPathDesktop,"processedTextforSTM.RData"))

ls()

colnames(meta)
summary(meta$date) ## last date 11/2020
summary(meta$meetingno) ## goes to 113

#################
##### Analysis
#### Search K over large number of potential topics
###########################

### searchk large

colnames(meta)

Ks=seq(from=10, to=100,by=10)

mod.tdsk <- searchK(documents=docs,
                       vocab=vocab,
                       K=Ks,
                    prevalence=~s(numdate)+
                    as.factor(income_level_iso3c),
                       data=meta,
                    seed=80620)

##save(mod.tdsk,
##     file=paste0(dataPathDesktop, "sK10to100M1to113.Rdata"))
 

### SearchK for a topic around K=20 that has an "emergencies" topic

KLows=seq(from=20, to=25,by=1)


mod.tdsk <- searchK(documents=docs,
                       vocab=vocab,
                       K=KLows,
                    prevalence=~s(numdate)+
                    as.factor(income_level_iso3c),
                       data=meta,
                    seed=80620)
