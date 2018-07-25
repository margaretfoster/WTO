###code to run search K
## on the trade and development
## meetings.

rm(list=ls())

library(stm)

dataPath<- "~/Dropbox/WTO/rdatas/"

data <- read.csv(paste0(dataPath, "paragraphsAndMeta.csv"))

processed <- textProcessor(documents=data$V2,
                           metadata=data)

summary(processed) ## 6548 x 5884 dictionary


out <- prepDocuments(processed$documents,
processed$vocab, processed$meta)

##267 docs, 165343 tokens

docs <- out$docs
vocab <- out$vocab
meta <- out$meta

set.seed(6889)
##first <- searchK(out$documents, out$vocab,
##                  K=c(5, 10, 15, 20, 25))

#save(first, file="tradeDevKsearch.Rdata")

second <- searchK(out$documents,
                  out$vocab,
                  prevalence=~docid + s(numdate), 
                  K=c(10:25),
                  data=out$meta)

save(second, file="tradDevKsearchParas1025.Rdata")
