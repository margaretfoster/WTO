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

summary(out$meta$meetingno) ## up to M 113
summary(out$meta$year)

## Merge "ADMN" into "NOTST" in the metadata

out$meta$iso3c.il <- out$meta$income_level_iso3c

out$meta[which(out$meta$iso3c.il=="ADMN"), "iso3c.il"] <-"NOTST"

table(out$meta$income_level_iso3c)
table(out$meta$iso3c.il)

############################
## Identifying the different shocks
##
colnames(out$meta)

head(out$meta$date)
head(out$meta$year)

length(docs)

## Pre/post China

out$meta$chinajoined <- ifelse(out$meta$year <= 2001,
                               0, 1)

summary(out$meta[which(
    out$meta$chinajoined==0),]$meetingno)
summary(out$meta[which(
    out$meta$chinajoined==1),]$year)

table(out$meta$chinajoined) ## 2710 0, 6143 as 1

table(out$meta$iso3c.il, out$meta$chinajoined)

## Pre/Post Financial Crisis

out$meta$postFC <- ifelse(out$meta$year <= 2008, 0, 1)

summary(out$meta[which(
    out$meta$postFC==0),]$meetingno)
summary(out$meta[which(
    out$meta$postFC==1),]$year)
## 72 meetings before 2008 Financial Crisis,2009-2020

table(out$meta$iso3c.il, out$meta$postFC)

## Pre-Post Xi

out$meta$postXi <- ifelse(out$meta$year <= 2012,
                          0, 1)

table(out$meta$iso3c.il, out$meta$postXi)

## Pre-Post Trump/right-win populists

out$meta$postTrump <- ifelse(out$meta$year <= 2016,
                             0, 1)
table(out$meta$iso3c.il, out$meta$postTrump)

############################
#### First run the model with
#### each shock as an intervention
############################

set.seed(61920)
k= 24

## modelChina <- ~s(numdate) * as.factor(iso3c.il) * as.factor(chinajoined)

## modelFC <- ~s(numdate) * as.factor(iso3c.il) * as.factor(postFC)

## modelXi <- ~s(numdate) * as.factor(iso3c.il) * as.factor(postXi)

## modelTrump <- ~s(numdate) * as.factor(iso3c.il) * as.factor(postTrump)

## Without numdate:

model2China <- ~as.factor(iso3c.il) * as.factor(chinajoined)

model2FC <- ~as.factor(iso3c.il) * as.factor(postFC)

model2Xi <-  ~as.factor(iso3c.il) * as.factor(postXi)

model2Trump <- ~as.factor(iso3c.il) * as.factor(postTrump)



levels(as.factor(out$meta$chinajoined))
levels(as.factor(out$meta$iso3c.il))

## K=24, model = China Joining

docs=out$documents
meta=out$meta
vocab=out$vocab

attributes(out)

mod2.out.china <- stm(documents=docs,
                     vocab=vocab,
                     data=meta,
                     K= k, ## 
                     prevalence=model2China,
               seed=61915)
 
prep2.china <- estimateEffect(c(1:k)~ as.factor(iso3c.il)*
                             as.factor(chinajoined),
                             mod2.out.china,
                             metadata=meta,
                             documents=docs,
                          uncertainty=c("Global"))

save(mod2.out.china, prep2.china, out,
     file=paste0(dataPathDesktop, "ChinaJoinShockEffect2.Rdata"))

#####################
## Financial Crisis
####################

mod2.out.FC <- stm(documents=docs,
                     vocab=vocab,
                     data=meta,
                     K= k, ## 
                     prevalence=model2FC,
               seed=61915)
 
prep2.FC <- estimateEffect(c(1:k)~as.factor(iso3c.il)*
                             as.factor(postFC),
                             mod2.out.FC,
                             metadata=meta,
                             documents=docs,
                          uncertainty=c("Global"))

save(mod2.out.FC, prep2.FC, out,
     file=paste0(dataPathDesktop, "FCShockEffect2.Rdata"))

ls()

################
### XI
################

mod2.out.Xi <- stm(documents=docs,
                  vocab=vocab,
                  data=meta,
                  K= k, ## 
                  prevalence=model2Xi,
                  seed=61915)

prep2.Xi <- estimateEffect(c(1:k)~ as.factor(iso3c.il)*
                             as.factor(postXi),
                             mod2.out.Xi,
                             metadata=meta,
                             documents=docs,
                          uncertainty=c("Global"))

save(mod2.out.Xi, prep2.Xi, out,
     file=paste0(dataPathDesktop, "XiEffect2.Rdata"))

######
### Trump
#######

mod2.out.Trump <- stm(documents=docs,
                  vocab=vocab,
                  data=meta,
                  K= k, ## 
                  prevalence=model2Trump,
                  seed=61915)

prep2.Trump <- estimateEffect(c(1:k)~as.factor(iso3c.il)*
                             as.factor(postTrump),
                             mod2.out.Trump,
                             metadata=meta,
                             documents=docs,
                          uncertainty=c("Global"))

save(mod2.out.Trump, prep2.Trump, out,
     file=paste0(dataPathDesktop, "TrumpEffect2.Rdata"))
