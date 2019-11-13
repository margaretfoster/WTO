## This script
## for the STM K sweeps:
## with word cutoffs at the 5% threshold

rm(list=ls())

##library(ggplot2)
##library(RColorBrewer)
library(stm)

## Load data:
load("WTOParagraphDataForSTM.Rdata")
colnames(paradata)


## Process the data:

colnames(paradata)

texts <- "paratext"
data <- paradata[, "paratext"]
metadata <- paradata[,!colnames(paradata) %in% texts]

processed <- textProcessor(paradata$paratext,
                           metadata = metadata)

el <- length(paradata$paratext) ## length shorthand

out <- prepDocuments(processed$documents,
                     processed$vocab,
                     processed$meta,
                     lower.thresh=el*.005, ## word must be in at least 5% of docs
                     upper.thresh=el*.85) ## 11290 = 13283 * .85
## upper.thresh removes words that appear in X number of documents
## here x is set at 85%. 85% doesn't remove any additional words;
## neither does 75% or 65%

## 5% of doc threshold removes 8476 of 8664 terms; .001 removed 6327  terms.
## often substantively interesting, but probably don't provide a lot of
## Information


grep("frustrat*", out$words.removed)

grep("frustr*", out$vocab)



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

save(out, file="WTObasemodel-out.Rdata")
### Sweep of topics
## also interesting for validation of the findings from K=50

Ksweep=c(5, 10, 15, 20, 25, 30)
Ksweep.large=c(15, 20, 25, 30, 35, 40, 45, 50) ## 9+ hoursY to run...
    
search.wide <- searchK(documents=out$documents,
                       data=out$meta,
                       vocab=out$vocab,
                       seed=1,
                       K=Ksweep,
                       content= ~ refbig5,
                       prevalence = ~ refbig5 * trump)
save(search.wide,
     file="WTOSearchKwide.Rdata")

load("WTOSearchKwide.Rdata")

##robustness of the "frustration" idea


trump.k10 <- stm(documents=out$documents,
                       data=out$meta,
                       vocab=out$vocab,
                       seed=689,
                       K=10,
                       content= ~ refbig5,
                       prevalence = ~ refbig5 * trump)


## what happens if I make pre-post Trump the
## content covariate, and who is referenced as the
## prevalence covariate?

inverse.trump.k10 <- stm(documents=out$documents,
                       data=out$meta,
                       vocab=out$vocab,
                       seed=689,
                       K=10,
                       content= ~ trump,
                       prevalence = ~ refbig5 * trump)


## what happens if the "content covariate"
## is whether a paragraph is pre or post trump/.


trump.k15 <- stm(documents=out$documents,
                       data=out$meta,
                       vocab=out$vocab,
                       seed=689,
                       K=15,
                       content= ~ refbig5,
                 prevalence = ~ refbig5 * trump)

labelTopics(trump.k15)
trump.k20 <- stm(documents=out$documents,
                       data=out$meta,
                       vocab=out$vocab,
                       seed=689,
                       K=20,
                       content= ~ refbig5,
                 prevalence = ~ refbig5 * trump)


trump.k25 <- stm(documents=out$documents,
                 data=out$meta,
                 vocab=out$vocab,
                 seed=689,
                 K=25,
                 content= ~ refbig5,
                 prevalence = ~ refbig5 * trump)


trump.k30 <- stm(documents=out$documents,
                 data=out$meta,
                 vocab=out$vocab,
                 seed=689,
                 K=30,
                 content= ~ refbig5,
                 prevalence = ~ refbig5 * trump)


trump.k35 <- stm(documents=out$documents,
                 data=out$meta,
                 vocab=out$vocab,
                 seed=689,
                 K=35,
                 content= ~ refbig5,
                 prevalence = ~ refbig5 * trump)


trump.k40 <- stm(documents=out$documents,
                 data=out$meta,
                 vocab=out$vocab,
                 seed=689,
                 K=40,
                 content= ~ refbig5,
                 prevalence = ~ refbig5 * trump)


trump.k45 <- stm(documents=out$documents,
                 data=out$meta,
                 vocabo=out$vocab,
                 seed=689,
                 K=45,
                 content= ~ refbig5,
                 prevalence = ~ refbig5 * trump)


inverse.trump.k45 <- stm(documents=out$documents,
                 data=out$meta,
                 vocab=out$vocab,
                 seed=689,
                 K=45,
                 content= ~ trump,
                 prevalence = ~ refbig5 * trump)

save.image(file="WTO_STM_Sweep.Rdata")

plot(search.wide) ## from 20-30, liklihood keeps increasing, slope narrows #
## but doesn't become negigible 


search.wide2 <- searchK(documents=out$documents,
                        data=out$meta,
                        vocab=out$vocab,
                        K=Ksweep.large,,
                        content= ~ refbig5,
                       prevalence = ~ refbig5 * trump)
 

save(search.wide2,
     file="WTOSearchkWideTo50.Rdata")

plot(search.wide2)

summary(search.wide2)



search.wide3 <- searchK(documents=out$documents,
                        data=out$meta,
                        vocab=out$vocab,
                        K=c(50, 55, 60, 65, 70, 75, 100),
                        content= ~ refbig5,
                        prevalence = ~ refbig5 * trump)

plot(search.wide3)

save(search.wide3,
     file="WTOSearchKwide3.Rdata")

###########
### Are results robust when
## estimated on corpus without
##US rep speaking


not.us <-which(!(paradata$speaker=="United States"))

texts <- "paratext"
data2 <- paradata[not.us, "paratext"]
metadata2 <- paradata[not.us,!colnames(paradata) %in% texts]

processed2 <- textProcessor(paradata[not.us,]$paratext,
                           metadata = metadata2)

el2 <- length(paradata[not.us,]$paratext) ## length shorthand

out2 <- prepDocuments(processed2$documents,
                     processed2$vocab,
                     processed2$meta,
                     lower.thresh=el2*.001, ## word must be in at least 5% of docs
                     upper.thresh=el2*.85) ## 11290 = 13283 * .85
## upper.thresh removes words that appear in X number of documents
## here x is set at 85%. 85% doesn't remove any additional words;
## neither does 75% or 65%

## 5% of doc threshold removes 8476 of 8664 terms; .001 removed 6327  terms.
## often substantively interesting, but probably don't provide a lot of
## Information


##out2$words.removed

docs2 <- out2$documents
vocab2 <- out2$vocab
meta2  <-out2$meta

out2$meta$refbig5 <- as.factor(out2$meta$refbig5)

bigFiveName <- levels(out2$meta$refbig5)

bigFiveName

## make a metadata row for factor
## of administration:

out2$meta$admin <- ifelse(out2$meta$clinton==1,"clinton",
                         ifelse(out2$meta$bush==1,"bush",
                                ifelse(out2$meta$obama==1,"obama",
                                       ifelse(out2$meta$trump==1,"trump",
                                              ifelse(out2$meta$bushobmtrans==1,"transition",
                                                     ifelse(out2$meta$obtrumptrans==1,"transition", "other"))))))

out2$meta$admin <- as.factor(out2$meta$admin)

summary(out2$meta$admin) ## no other, which is good

table(out2$meta$speaker) ## no US!


#### on data without US:

trump.nous.k50 <- stm(documents=out2$documents,
                       data=out2$meta,
                       vocab=out2$vocab,
                       seed=689,
                       K=50,
                       content= ~ refbig5,
                      prevalence = ~ refbig5 * trump)


trump.nous.k45 <- stm(documents=out2$documents,
                       data=out2$meta,
                       vocab=out2$vocab,
                       seed=689,
                       K=45,
                       content= ~ refbig5,
                       prevalence = ~ refbig5 * trump)

trump.nous.k40 <- stm(documents=out2$documents,
                      data=out2$meta,
                      vocab=out2$vocab,
                      seed=689,
                      K=40,
                      content= ~ refbig5,
                      prevalence = ~ refbig5 * trump)

trump.nous.k35 <- stm(documents=out2$documents,
                       data=out2$meta,
                       vocab=out2$vocab,
                       seed=689,
                       K=35,
                       content= ~ refbig5,
                      prevalence = ~ refbig5 * trump)

trump.nous.k30 <- stm(documents=out2$documents,
                       data=out2$meta,
                       vocab=out2$vocab,
                       seed=689,
                       K=30,
                       content= ~ refbig5,
                      prevalence = ~ refbig5 * trump)


## What documents is the frustrate/oppose result picking up in the whole-reference data?

ls()

dim(paradata) ## 13811

grep(pattern="oppose", paradata)

grep("oppose", paradata)
grep("frustrat*", paradata)

paradata[grepl("frustrat*", paradata$paratext), c("paratext", "refs", "date")]

paradata[grepl("oppos*", paradata$paratext), c("paratext", "refs", "date")
