###code to run search K
## on the trade and development
## meetings.

rm(list=ls())

library(stm)
library(xtable)

dataPath<- "./"


load("tradeandDevSTMOut.Rdata")

set.seed(6889)
##first <- searchK(out$documents, out$vocab,
##                  K=c(5, 10, 15, 20, 25))

#save(first, file="tradeDevKsearch.Rdata")

## see if any missing data:
sapply(out$meta, length) ## all 13284


second <- searchK(out$documents,
                  out$vocab,
                  prevalence=~docid + s(numdate), 
                  K=c(10:25),
                  data=out$meta)

save(second, file="tradDevKsearchParas1025.Rdata")
