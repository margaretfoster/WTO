###code to run search K
## on the trade and development
## meetings.

rm(list=ls())

library(tm)
library(quanteda)

filepath0 <- "../TandDdownloads/ENGLISH/"

files0 <- list.files(path=filepath0,
                    pattern="pdf$")

readpdf <- readPDF(control = list(text = "-layout"))

tradeAndDev <- Corpus(URISource(paste0(filepath0, files0)), 
                   readerControl = list(reader = readpdf))

###### tradeAndDev

test <- corpus(tradeAndDev)

class(test)

library(stm)

processed <- textProcessor(test)

summary(processed) ## 267 x 17137 dictionary


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

second <- searchK(out$documents, out$vocab,
                  K=c(15:23))

save(second, file="tradDevKsearch1523.Rdata")
