
### Run some STM on the trade and development meetings
## just the subset of paragraphs with 50 or more words

rm(list=ls())

library(stm)

dataPath<- "./"

savePath <- "../analysis/"

####################################
## Load processed data
###################################

## censored data:
## only paragraphs more than 50 words
load("tradeandDevSTMOut50Plus.Rdata")

####################################
## STM
####################################
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


mod.out.20.cens2 <- stm(documents=docscens,
               vocab=vocabcens,
               data=metacens,
               K=20,
               prevalence=~s(numdate)*country.speaker,
               seed=6889)

 
save(mod.out.20.cens2,
     file=paste0(savePath, "tradDevParaK20cens2.RData"))

## load(paste0(savePath, "tradDevParaK20cens2.RData"))

prep20cens <- estimateEffect(c(1:20)~s(numdate) + docid,
                       mod.out.20.cens,
                       metadata=metacens, documents=docscens,
                       uncertainty=c("Global"))

save(prep20cens,
     file=paste0(savePath, "estimateEffectTDK20cens.Rdata"))


prep20censlarge <- estimateEffect(c(1:20)~s(numdate)* country.speaker,
                       mod.out.20.cens2,
                       metadata=metacens,
                       documents=docscens,
                       uncertainty=c("Global"))
save(prep20censlarge,
     file=paste0(savePath, "estimateEffectTDK20censlarge.Rdata"))
