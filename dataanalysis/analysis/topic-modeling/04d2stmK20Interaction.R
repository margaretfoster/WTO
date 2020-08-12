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

#############################
## Load Processed Data
#############################

load(paste0(dataPathDesktop,"processedTextforSTM.Rdata"))

############################
##### Analysis
############################

colnames(out$meta)
table(out$meta$delttype)

############################
#### How does K=20 look?
############################

set.seed(61920)

mod.out.20 <- stm(documents=docs,
               vocab=vocab,
               data=meta,
               K=20, ## 
                  prevalence= ~ s(numdate) *
                  as.factor(delttype),
               seed=61920)
 

prep.20 <- estimateEffect(c(1:20)~ s(numdate) *
                          as.factor(delttype),
                       mod.out.20,
                       metadata=meta,
                       documents=docs,
                       uncertainty=c("Global"))

save.image(file=paste0(dataPathDesktop,
               "tradDevPara_20InteractDeltType.RData"))
