## This script generates data for a structural topic model
## and runs ksweep search in the 40-50 range:
#
rm(list=ls())

##library(ggplot2)
##library(RColorBrewer)
library(stm)

load("WTOParagraphDataForSTM.Rdata")


## Data:
##

## Make sure there are no missing values:

print(paste0("There are ",
             length(which(is.na(paradata))),
             " NA values in the data"))


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


#### Search K b/c no prior on optimal number of
## topics to look for:

## focusing on fit in the 40-50 topic range:
ksweep <- seq(from=40, to=50, by=1)

s.w <- searchK(documents=out$documents,
              data=out$meta,
              vocab=out$vocab,
              K=ksweep,
              content= ~ refbig5,
              prevalence = ~ refbig5 * trump)

save(s.w,
     file="WTOSearchKwide40to50.Rdata")

print("saved")
