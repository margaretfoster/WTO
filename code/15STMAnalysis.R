## This script to generate the
## data needed for a structural topic model
## with covariates:
## US Administration
## Trade Rep?
## (and new representative)


rm(list=ls())

##library(ggplot2)
##library(RColorBrewer)
library(stm)

## Load data:
load("WTOParagraphDataForSTM.Rdata")
colnames(paradata)


## Process the data:

which(is.na(paradata)) ## no nas here

colnames(paradata)

texts <- "paratext"
data <- paradata[, "paratext"]
metadata <- paradata[,!colnames(paradata) %in% texts]

processed <- textProcessor(paradata$paratext,
                           metadata = metadata)


out <- prepDocuments(processed$documents,
                     processed$vocab,
                     processed$meta,
                     lower.thresh=13282*.001, ## word must be in at least 5% of docs
                     upper.thresh=13283*.85) ## 11290 = 13283 * .85
## upper.thresh removes words that appear in X number of documents
## here x is set at 85%. 85% doesn't remove any additional words;
## neither does 75% or 65%

## 5% of doc threshold removes 8476 of 8664 terms; .001 removed 6327  terms.
## often substantively interesting, but probably don't provide a lot of
## Information


out$words.removed

docs <- out$documents
vocab <- out$vocab
meta  <-out$meta

out$meta$refbig5 <- as.factor(out$meta$refbig5)

bigFiveName <- levels(out$meta$refbig5)


## make a metadata row for factor
## of administration:

out$meta$admin <- ifelse(out$meta$clinton==1,"clinton",
                         ifelse(out$meta$bush==1,"bush",
                                ifelse(out$meta$obama==1,"obama",
                                       ifelse(out$meta$trump==1,"trump",
                                              ifelse(out$meta$bushobmtrans==1,"transition",
                                                     ifelse(out$meta$obtrumptrans==1,"transition", "other"))))))

out$meta$admin <- as.factor(out$meta$admin)

summary(out$meta$admin) ## no other, which is good



                         
## Because no priors,  start with the built-in
## STM spectral, K=0 and see what we get
#### Search K b/c no prior on optimal number of
## topics to look for:

## With this seed: 50 topics, and aspect 6 (US, post trump) words are
## frustration words
nopriorfit <- stm(documents=out$documents,
                   data=out$meta,
                   vocab=out$vocab,
                   seed=6889,
                   K=0, ## will search for what the algorithm propses
                   init.type="Spectral",
                   content= ~ refbig5,
                   prevalence = ~ trump*refbig5)


save(nopriorfit,
     file="noprioronfitWTOSTM.Rdata")

## Full model:


## with dummy for paragraphs that are in the
## trump admin
nopriorfit.mod <- stm(documents=out$documents,
                      data=out$meta,
                      vocab=out$vocab,
                      seed=6889,
                      K=50, 
                      init.type="Spectral",
                      content= ~ refbig5,
                      prevalence = ~ trump*refbig5)


## with administration as a factor
## rather than a dummy for Trump Admin
nopriorfit.mod2 <- stm(documents=out$documents,
                       data=out$meta,
                       vocab=out$vocab,
                       seed=6889,
                       K=50, 
                       init.type="Spectral",
                       content= ~ refbig5,
                       prevalence = ~ admin*refbig5)


save(nopriorfit.mod,
     file="nopriorfit_mod.Rdata")

save(nopriorfit.mod2,
     file="nopriorfit_adminmod.Rdata")
