### Run some STM on the trade and development meetings

rm(list=ls())

library(stm)

dataPath<- "./"

savePath <- "../analysis/"

####################################
## Load processed data
###################################


## full data:
load("tradeandDevSTMOut.Rdata")

## censored data:
## only paragraphs more than 50 words
load("tradeandDevSTMOut50Plus.Rdata")

load(paste0(savePath, "tradDevParaLev10cens.RData"))

####################################
## STM
####################################

docsfull <- out$documents
vocabfull <- out$vocab
metafull <- out$meta

set.seed(6889)

mod.out.10 <- stm(documents=docsfull,
               vocab=vocabfull,
               data=metafull,
               K=10,
               prevalence=~s(numdate)+ docid,
               seed=6889)

 
save(mod.out.10,
     file=paste0(savePath, "tradDevParaLev10.RData"))


prep10 <- estimateEffect(c(1:10)~s(numdate) + docid,
                       mod.out.10,
                       metadata=metafull, documents=docsfull,
                       uncertainty=c("Global"))

save(prep10,
     file=paste0(savePath, "estimateEffectParaLevTD10.Rdata"))

#### Censored data

docscens <- outcens$documents
vocabcens <- outcens$vocab
metacens <- outcens$meta

set.seed(6889)

mod.out.10.cens <- stm(documents=docscens,
               vocab=vocabcens,
               data=metacens,
               K=10,
               prevalence=~s(numdate)+ docid,
               seed=6889)

 
save(mod.out.10.cens,
     file=paste0(savePath, "tradDevParaLev10cens.RData"))

prep10cens <- estimateEffect(c(1:10)~s(numdate) + docid,
                       mod.out.10.cens,
                       metadata=metacens, documents=docscens,
                       uncertainty=c("Global"))

save(prep10cens,
     file=paste0(savePath, "estimateEffectParaLevTD10cens.Rdata"))


### Update 3/19: reviewing the data:

load("../analysis/tradDevParaLev10.RData")

ls()

summary(mod.out.10)

ls()

colnames(metafull)


unique(metafull$country.speaker)
