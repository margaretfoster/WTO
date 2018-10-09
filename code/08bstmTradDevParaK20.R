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

####################################
## STM
####################################

docsfull <- out$documents
vocabfull <- out$vocab
metafull <- out$meta

set.seed(6889)

mod.out.20 <- stm(documents=docsfull,
               vocab=vocabfull,
               data=metafull,
               K=20,
               prevalence=~s(numdate)+ docid,
               seed=6889)

 
save(mod.out.20,
     file=paste0(savePath, "tradDevParaK20.RData"))

prep20 <- estimateEffect(c(1:20)~s(numdate) + docid,
                       mod.out.20,
                       metadata=metafull, documents=docsfull,
                       uncertainty=c("Global"))

save(prep20,
     file=paste0(savePath, "estimateEffectTDK20.Rdata"))

#### Censored data


docscens <- outcens$documents
vocabcens <- outcens$vocab
metacens <- outcens$meta

set.seed(6889)

mod.out.20.cens <- stm(documents=docscens,
               vocab=vocabcens,
               data=metacens,
               K=20,
               prevalence=~s(numdate)+ docid,
               seed=6889)

 
save(mod.out.20.cens,
     file=paste0(savePath, "tradDevParaK20cens.RData"))

prep20cens <- estimateEffect(c(1:20)~s(numdate) + docid,
                       mod.out.20.cens,
                       metadata=metacens, documents=docscens,
                       uncertainty=c("Global"))

save(prep20cens,
     file=paste0(savePath, "estimateEffectTDK20cens.Rdata"))
