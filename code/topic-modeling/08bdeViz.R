

### Run some STM on the trade and development meetings

rm(list=ls())

library(stm)

dataPath<- "./"

## savePath <- "../analysis/"

####################################
## Load processed data
###################################

## censored data:
## only paragraphs more than 50 words
load("tradeandDevSTMOut50Plus.Rdata")

## STM model, 100 iterations, K=5,
## prevalence: numdate * country.speaker

load("tradDevParaK20cens2.RData")
## estimate effect K=5, controling for time and speaker:
## prevalence = ~ numdate*country.speaker
load("estimateEffectTDK20censlarge.Rdata")

## prepcenslarge = the estimate effect
## mod.out.20.cens2 = STM model

pdf(file="stmK5time.pdf")


pdf(file="testEstEffect.pdf")
plot(prep20censlarge,
     covariate="country.speaker",
     cov.value="")
dev.off()

