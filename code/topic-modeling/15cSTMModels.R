## This script to make STM models
## Follows from 15 (data prep and searchK)
## and 15b (plotting the searchK data)


rm(list=ls())

##library(ggplot2)
##library(RColorBrewer)
library(stm)

load("WTOParagraphDataForSTM.Rdata")

colnames(paradata)


## Data:
##

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

################


model1 <- stm(documents=out$documents,
              data=out$meta,
              vocab=out$vocab,
              K=35,
              seed=6889,
              content= ~ refbig5,
              prevalence = ~ refbig5 * trump) 

summary(model1)
plot(model1)

prep <- estimateEffect(1:10 ~ refbig5 * trump,
                       model1,
                       meta = out$meta,
                       uncertainty = "Global")

sagelabs <- sageLabels(model1, 10)


plot(prep, covariate="refbig5",
     model= model1,
     moderator="trump",
     moderator.value=0)

summary(model1)

plot(model1)

cors <- topicCorr(model1,
                  method="simple",
                  cutoff=.15)

set.seed(6889)
plot(cors,
     vertex.size=.75,
     color="gray"
     )

####
## k=10 doensn't really tell me a story
## same with k=15, looks like a bunch of stray words in some of the country-topic covariates, and some are empty for a country...


modelk30 <- stm(documents=out$documents,
              data=out$meta,
              vocab=out$vocab,
              K=30,
              content= ~ refbig5,
              prevalence = ~ refbig5 * trump) 

summary(modelk30)

plot(modelk30)


modelk40 <- stm(documents=out$documents,
              data=out$meta,
              vocab=out$vocab,
              K=40,
              content= ~ refbig5,
              prevalence = ~ refbig5 * trump) 

summary(modelk40)

plot(modelk30)


## human-interpretable:
modelk9 <- stm(documents=out$documents,
              data=out$meta,
              vocab=out$vocab,
              K=9,
              content= ~ refbig5,
              prevalence = ~ refbig5 * trump) 

summary(modelk9)

plot(modelk30)


## changes in effect by reference to


dev.off()

plot(prep, "refbig5",
     topics=c(10),
     method="pointestimate",
     labeltype="custom",
     custom.labels=bigFiveName,
     xlab="Mean topic proportion in corpus")

## cov.betas in sageLabels
dev.off()

par(mfrow=c(1, 2))

plot(prep, "trump",
     method="difference",
     moderator="refbig5",
     moderator.value="United States",
     cov.value1=0,
     cov.value2=1,
     model=model1,
     verbose.labels = FALSE,
     xlab="Change in Topics In Paras Referencing US")


plot(prep, "trump",
     model=model1,
     ##method="continuous",
     moderator="refbig5",
     moderator.value="China",
     method="difference",
     cov.value1=0,
     cov.value2=1,
##     add=TRUE,
   ## printlabel=FALSE,
     verbose.labels = FALSE,
     xlab="Change in Topics In Paras Referencing China)")



plot(model1,
     type="perspectives",
     covarlevels = c(0, 1),
     topics=c(7, 9))


##Using Brandon Stewart's code
## From Text for Roberts Et Al,
## "A Model of Text for Experimentation in the Social Sciences"

dev.off()

plotQuote(c(paste(sagelabs$cov.betas[[2]]$problabels[10,], collapse="\n"),
            paste(sagelabs$cov.betas[[6]]$problabels[10,], collapse="\n")))
text(x=2.5,
     y=6.0,
     "Topic 10 Words in Pararaphs Referring to China", cex=1.1)
text(x=2.5,
     y=3.2,
     "Topic 10 Words in Paragraphs Referring to United States", cex=1.1)



### Replicating Roberts et al figure 7:
## for my topic 10

dev.off()

plotQuote(c(paste(sagelabs$cov.betas[[1]]$problabels[10,], collapse="\n"),
            paste(sagelabs$cov.betas[[2]]$problabels[10,], collapse="\n"),
            paste(sagelabs$cov.betas[[3]]$problabels[10,], collapse="\n"),
            paste(sagelabs$cov.betas[[4]]$problabels[10,], collapse="\n"),
            paste(sagelabs$cov.betas[[6]]$problabels[10,], collapse="\n")), width=40)
text(.5,4.5,"Ref to Canada", cex=1.1)
text(.5,3.5,"Ref to China", cex=1.1)
text(.5,2.5,"Ref to Egypt", cex=1.1)
text(.5,1.5,"Ref to India", cex=1.1)
text(.5,0.5,"Ref to US", cex=1.1)


ls()


plot(sagelabs)
