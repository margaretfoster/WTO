### Preliminary analysis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

## This specification with non-overlapping
## shock periods
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
## Non-overlapping

colnames(out$meta)

head(out$meta$date)
head(out$meta$year)

length(docs)

## Pre/post China Join Period
## 2002-2008
out$meta$Chinaperiod <- ifelse(out$meta$year <= 2001 |
                               out$meta$year >= 2008   ,
                               0, 1)

summary(out$meta[which(
    out$meta$Chinaperiod==0),]$meetingno)
summary(out$meta[which(
    out$meta$Chinaperiod==1),]$year)

table(out$meta$Chinaperiod) ## 2710 0, 6143 as 1

table(out$meta$iso3c.il, out$meta$Chinaperiod)

## Financial Crisis Period
## 2008-2012

out$meta$FCperiod <- ifelse(out$meta$year <= 2008 |
                          out$meta$year >= 2012, 0, 1)


summary(out$meta[which(
    out$meta$FCperiod==1),]$year)

table(out$meta$iso3c.il, out$meta$FCperiod)

## Xi Period
## 2013- 2016
out$meta$Xiperiod <- ifelse(out$meta$year <= 2012 |
                            out$meta$year > 2016,
                          0, 1)

summary(out$meta[which(
    out$meta$Xiperiod==1),]$year)

table(out$meta$iso3c.il, out$meta$Xiperiod)

## Trump/RW Populist Period
## 2016-2020

out$meta$Trumpperiod <- ifelse(out$meta$year <= 2016 |
                             out$meta$year > 2020,
                             0, 1)

summary(out$meta[which(
    out$meta$Trumpperiod==1),]$year)

table(out$meta$iso3c.il, out$meta$Trumpperiod)

## Covid period
## 2020

out$meta$covidperiod <- ifelse(out$meta$year <= 2016 |
                             out$meta$year > 2020,
                             0, 1)

summary(out$meta[which(
    out$meta$covidperiod==1),]$year)

table(out$meta$iso3c.il, out$meta$covidperiod)

############################
#### First run the model with
#### each shock as an intervention
############################

set.seed(61920)
k= 24

model2China <- ~as.factor(iso3c.il) * as.factor(Chinaperiod)

model2FC <- ~as.factor(iso3c.il) * as.factor(FCperiod)

model2Xi <-  ~as.factor(iso3c.il) * as.factor(Xiperiod)

model2Trump <- ~as.factor(iso3c.il) * as.factor(Trumpperiod)

model2covid <- ~as.factor(iso3c.il) * as.factor(covidperiod)


levels(as.factor(out$meta$Chinaperiod))
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
                             as.factor(Chinaperiod),
                             mod2.out.china,
                             metadata=meta,
                             documents=docs,
                          uncertainty=c("Global"))

save(mod2.out.china, prep2.china, out,
     file=paste0(dataPathDesktop, "ChinaJoinShockEffectAlt.Rdata"))

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
                             as.factor(FCperiod),
                             mod2.out.FC,
                             metadata=meta,
                             documents=docs,
                          uncertainty=c("Global"))

save(mod2.out.FC, prep2.FC, out,
     file=paste0(dataPathDesktop, "FCShockEffectAlt.Rdata"))

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
                             as.factor(Xiperiod),
                             mod2.out.Xi,
                             metadata=meta,
                             documents=docs,
                          uncertainty=c("Global"))

save(mod2.out.Xi, prep2.Xi, out,
     file=paste0(dataPathDesktop, "XiEffectAlt.Rdata"))

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
                             as.factor(Trumpperiod),
                             mod2.out.Trump,
                             metadata=meta,
                             documents=docs,
                          uncertainty=c("Global"))

save(mod2.out.Trump, prep2.Trump, out,
     file=paste0(dataPathDesktop, "TrumpEffectAlt.Rdata"))



#########
## Covid
##########
mod2.out.covid <- stm(documents=docs,
                  vocab=vocab,
                  data=meta,
                  K= k, ## 
                  prevalence=model2covid,
                  seed=61915)

prep2.covid <- estimateEffect(c(1:k)~as.factor(iso3c.il)*
                             as.factor(covidperiod),
                             mod2.out.covid,
                             metadata=meta,
                             documents=docs,
                          uncertainty=c("Global"))

save(mod2.out.covid, prep2.covid, out,
     file=paste0(dataPathDesktop, "covidEffectAlt.Rdata"))
