## This script to generate the
## data needed for a structural topic model
## with covariates:
## US Administration
## Trade Rep?
## (and new representative)

## but removing the paragraphs where the "references"
## are just to the speaker

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

## Remove the ~3k paragraphs that are just
## references to the speaker:

paradat <- subset(paradata, !(paradata$speaker == paradata$refs))
dim(paradat)
dim(paradata)


texts <- "paratext"
data <- paradat[, "paratext"]
metadata <- paradat[,!colnames(paradat) %in% texts]


##write.csv(metadata,file="WTO_No_Self_References.csv")

processed <- textProcessor(paradat$paratext,
                           metadata = metadata,
                           customstopwords = c("frustrate",
                               "united", "states","canada",
                               "china", "egypt", "india"))

el <- length(data)

out <- prepDocuments(processed$documents,
                     processed$vocab,
                     processed$meta,
                     lower.thresh=el*.001, ## word must be in at least 5% of docs
                     upper.thresh=el*.85) ## 11290 = 13283 * .85

### In this no-loop data, threashold= 10301 documents, 2337 terms


removed <- out$words.removed

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


## features of senders/recievers according to admin:

colnames(out$meta)

## Frequency of speakers by administration:





### Topic Modeling:




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


sink(file="no.loops.unsupervisedk.txt")
summary(nopriorfit)
sink() 
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

sink(file="no.loops.k50.txt")
summary(nopriorfit.mod)
sink()

## with administration as a factor
## rather than a dummy for Trump Admin

names(out$meta$admin) <- levels(out$meta$admin)

nopriorfit.adminmod <- stm(documents=out$documents,
                           data=out$meta,
                           vocab=out$vocab,
                           seed=6889,
                           K=50, 
                           init.type="Spectral",
                           content= ~ admin, ## content varies by administration
                           prevalence = ~ admin*refbig5) ## topic prevalence varies by country


sink(file="adminmod-k50-noloops.txt")
labelTopics(nopriorfit.adminmod)
sink()

## So, would want to do estimate effect, prevalence covariate = US;
## content = before and after trump

prep <- estimateEffect(formula=1:50~ admin*refbig5,
                       nopriorfit.adminmod,
                       out$meta,
                       #documents=out$documents,
                       uncertainty="Global")






save(nopriorfit,
     file="noloops-noprioronfitWTOSTM.Rdata")


save(nopriorfit.nl.mod,
     file="nl-nopriorfit_mod.Rdata")

save(nopriorfit.adminmod,
     file="nl-nopriorfit_adminmod.Rdata")
