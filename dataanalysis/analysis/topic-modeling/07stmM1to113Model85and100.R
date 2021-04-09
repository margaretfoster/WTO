
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
    dataPathDesktop <- "~/Dropbox/WTO-Data/rdatas/"
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
### Fresh run of the 85 and 100 topic models
### 4/9/21
###########################

formul <- ~s(numdate)* as.factor(income_level_iso3c)
Ks=85

mod.85 <- stm(documents=docs,
              vocab=vocab,
              K=Ks,
              prevalence=formul,
              data=meta,
              seed=80620)

prep.85 <- estimatEffect(
    formula=formul,
    stmobj=mod.85,
    metadata=meta,
    documents=docs)

save(out, mod.85, prep.85,
     file=paste0(dataPathDesktop, "K85model.Rdata"))
 

## 100

Ks=100

mod.100 <- stm(documents=docs,
              vocab=vocab,
              K=Ks,
              prevalence=formul,
              data=meta,
              seed=80620)  

prep.100 <- estimateEffect(
    formula=formul,
    stmobj=mod.100,
    metadata=meta,
    documents=docs)

save(out, mod.100, prep.100,
     file=paste0(dataPathDesktop, "K100model.Rdata"))
