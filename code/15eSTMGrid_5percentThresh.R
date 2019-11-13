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

save(out, file="WTObasemodel-out-5percent.Rdata")
### Sweep of topics
## also interesting for validation of the findings from K=50

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

save.image(file="WTO_STM_Sweep_5percent.Rdata")

