### Preliminary analysis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 'pdftools',
           'tidyr', 'quanteda')

loadPkg(packs)


#########################
## Declare Data Paths
#########################

if(Sys.info()['user']=="Ergane"){ ## if on my own machine look in Dropbox
    dataPathDesktop <- "~/Dropbox/WTO-Data/rdatas/"
    print(paste0("The datapath is: ", dataPathDesktop))
}else{ ## else look in ~/WTO/
    dataPathDesktop <- "../../"
    print(paste0("The datapath is: ", dataPathDesktop))
}

#############################
## Load Processed Data
#############################

load(paste0(dataPathDesktop,"processedTextforSTM.Rdata"))

############################
##### Analysis
############################

colnames(out$meta)

summary(out$meta$meetingno) ## up to 113

############################
#### How does K=20 look?
############################

set.seed(61920)

model <- ~s(numdate) * as.factor(income_level_iso3c)

## K=15
mod.out.15 <- stm(documents=docs,
               vocab=vocab,
               data=meta,
                  K=15, ## 
                  prevalence= model,
               seed=61915)
 
prep.15 <- estimateEffect(c(1:15)~s(numdate) * as.factor(income_level_iso3c),
                       mod.out.15,
                       metadata=meta,
                       documents=docs,
                          uncertainty=c("Global"))

## K=20
mod.out.20 <- stm(documents=docs,
               vocab=vocab,
               data=meta,
                  K=20, ## 
                  prevalence= model,
               seed=61920)
 
prep.20 <- estimateEffect(c(1:20) ~s(numdate) * as.factor(income_level_iso3c),
                       mod.out.20,
                       metadata=meta,
                       documents=docs,
                       uncertainty=c("Global"))

## K=22
mod.out.22 <- stm(documents=docs,
               vocab=vocab,
               data=meta,
                  K=22, ## 
                  prevalence= model,
               seed=61920)
 
prep.22 <- estimateEffect(c(1:22) ~s(numdate) * as.factor(income_level_iso3c),
                       mod.out.22,
                       metadata=meta,
                       documents=docs,
                       uncertainty=c("Global"))

## K=23
mod.out.23 <- stm(documents=docs,
               vocab=vocab,
               data=meta,
                  K=23, ## 
                  prevalence= model,
               seed=61920)
 
prep.23 <- estimateEffect(c(1:23) ~s(numdate) * as.factor(income_level_iso3c),
                       mod.out.23,
                       metadata=meta,
                       documents=docs,
                       uncertainty=c("Global"))

## K=23
mod.out.24 <- stm(documents=docs,
               vocab=vocab,
               data=meta,
                  K=24, ## 
                  prevalence= model,
               seed=61920)
 
prep.24 <- estimateEffect(c(1:24) ~s(numdate) * as.factor(income_level_iso3c),
                       mod.out.24,
                       metadata=meta,
                       documents=docs,
                       uncertainty=c("Global"))

## K=25
mod.out.25 <- stm(documents=docs,
               vocab=vocab,
               data=meta,
                  K=25, ## 
                  prevalence= model,
               seed=61925)
 
prep.25 <- estimateEffect(c(1:25)  ~s(numdate) * as.factor(income_level_iso3c),
                       mod.out.25,
                       metadata=meta,
                       documents=docs,
                       uncertainty=c("Global"))


save.image(file=paste0(dataPathDesktop,
               "tradDevPara_15to25M113.RData"))
