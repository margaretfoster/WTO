## This script to
## run a topic model
## on just US -referring paragraphs
## with covariate for administration
## and thus to validate findings
## from larger model

rm(list=ls())

##library(ggplot2)
##library(RColorBrewer)
library(stm)

load("WTOParagraphDataForSTM.Rdata")
colnames(paradata)

##############
## Data Processing
################

## only those paragraphs
## referring to the US:
usonly <- paradata[which(paradata$reftoUS==1),]

texts <- "paratext"
data <- usonly[, "paratext"]

metadata <- usonly[,!colnames(paradata) %in% texts]

processed <- textProcessor(usonly$paratext,
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

levels(out$meta$admin)
## bush, clinton, obama, transition, trump

## no priors on number of topics,
## so once again using the unsupervised:

## but the covariates now are admin
adminSearch <- searchK(documents=out$documents,
                   data=out$meta,
                   vocab=out$vocab,
                   seed=610,
                   K=c(5, 10, 15, 20, 25), ## unsupervised search
                   init.type="Spectral",
                   content= ~ admin,
                   prevalence = ~ admin)


adminSearch3 <- searchK(documents=out$documents,
                        data=out$meta,
                        vocab=out$vocab,
                        seed=610,
                        K=c(30, 35, 40, 45, 50), ## unsupervised search
                        init.type="Spectral",
                        content= ~ admin,
                        prevalence = ~ admin)

## Full model:


## inspect the Trump paragraphs


class(usonly)
colnames(usonly)

## inspect Trump paragraphs:
## write to CSV for ease of inspection


write.csv(usonly[which(usonly$trump==1),
                 c("meeting", "paratext")],
          file="refsToUSTrump.csv")

## inspection reveals that these are also paragraphs in which
## the US is the speaker:

colnames(usonly)

table(usonly$speaker)
which(!(usonly$speaker=="United States"))
tail(usonly$speaker)
dim(usonly)
usonly[828, "speaker"]

usonly1 <- usonly[which(!(usonly$speaker=="United States")),]


texts <- "paratext"
data <- usonly1[, "paratext"]

metadata <- usonly1[,!colnames(paradata) %in% texts]

processed <- textProcessor(usonly1$paratext,
                           metadata = metadata)


out2 <- prepDocuments(processed$documents,
                     processed$vocab,
                     processed$meta,
                     lower.thresh=261*.001, ## word must be in at least 5% of docs
                     upper.thresh=261*.85) ## 11290 = 13283 * .85
## upper.thresh removes words that appear in X number of documents
## here x is set at 85%. 85% doesn't remove any additional words;
## neither does 75% or 65%

## 5% of doc threshold removes 8476 of 8664 terms; .001 removed 6327  terms.
## often substantively interesting, but probably don't provide a lot of
## Information


out2$words.removed

docs <- out2$documents
vocab <- out2$vocab
meta  <-out2$meta

out2$meta$refbig5 <- as.factor(out2$meta$refbig5)
bigFiveName <- levels(out2$meta$refbig5)


## make a metadata row for factor
## of administration:

out2$meta$admin <- ifelse(out2$meta$clinton==1,"clinton",
                         ifelse(out2$meta$bush==1,"bush",
                                ifelse(out2$meta$obama==1,"obama",
                                       ifelse(out2$meta$trump==1,"trump",
                                              ifelse(out2$meta$bushobmtrans==1,"transition",
                                                     ifelse(out2$meta$obtrumptrans==1,"transition", "other"))))))

out2$meta$admin <- as.factor(out2$meta$admin)



adminSearch2 <- searchK(documents=out2$documents,
                   data=out2$meta,
                   vocab=out2$vocab,
                   seed=610,
                   K=c(5, 10, 15, 20, 25), ## unsupervised search
                   init.type="Spectral",
                   content= ~ admin,
                   prevalence = ~ admin)
