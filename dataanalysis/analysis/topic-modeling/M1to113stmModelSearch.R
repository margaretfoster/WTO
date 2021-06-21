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

############################
#### After some exploration
### narrowing on K=21
## (closest to the M1-109 model)
## And K=85, which is the first time that a
## distinct pandemic topic shows up in a
## "sweep from 50-100 by 25 than 10" search
############################

set.seed(61920)
### does Topic 21 give us a "pandemic" topic?
## Note that topic 21 is more coherent for the M1-113 data than
## either K=20 or K=22, 
## but no pandemic topic (instead, the vulnerable economies topic)

## formula: time interacted with delegation [nationa] income level:

form= ~s(numdate) * as.factor(income_level_iso3c)

mod.out.21 <- stm(documents=docs,
                  vocab=vocab,
                  data=meta,
                  K=21, ## 
                  prevalence=form ,
               seed=61921)
 

prep.21 <- estimateEffect(c(1:21) ~s(numdate) *
                          as.factor(income_level_iso3c),
                          mod.out.21,
                          metadata=meta,
                          documents=docs,
                          uncertainty=c("Global"))

summary(mod.out.21) ## Coherent, but doesn't have a specific pandemic topic
 
### does a huge number of topics produce  "pandemic" content?
## A: yes.
        
mod.out.100 <- stm(documents=docs,
                   vocab=vocab,
                   data=meta,
                   K=100, ## 
                   prevalence=form,
                   seed=61921)
 

prep.100 <- estimateEffect(c(1:100) ~s(numdate) *
                           as.factor(income_level_iso3c),
                           mod.out.100,
                           metadata=meta,
                           documents=docs,
                           uncertainty=c("Global"))


summary(mod.out.100) ## has a pandemic topic, very high number of topics

#### (Deleted: searched K50 and K75 for a "pandemic" topic)
### K75 has "pandemic" in a frex word, but the topics themselves were not
## very coherent

mod.out.85 <- stm(documents=docs,
                  vocab=vocab,
                  data=meta,
                  K=85, ## 
                  prevalence=form,
               seed=61921)
 

prep.85 <- estimateEffect(c(1:85)~ s(numdate) *
                          as.factor(income_level_iso3c),
                          mod.out.85,
                       metadata=meta,
                          documents=docs,
                       uncertainty=c("Global"))


summary(mod.out.85) ## There is a distinctive "pandemic"
## topic in 85

save.image(file=paste0(dataPathDesktop,
               "tradDevParaM1to113-Pandemic.RData"))
