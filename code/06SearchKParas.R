###code to run search K
## on the trade and development
## meetings.

rm(list=ls())

library(stm)
library(xtable)

dataPath<- "./"

## wto1 is the dataframe, it has speakers extracted for each
## paragraph, dates extracted, full text for the paragraph 
##  meeting ID and paragraph #

load("wtoTDMinutes.Rdata")

ls()

class(wto1)
dim(wto1)

colnames(wto1)

## remove unused column:o
wto1$extradates <- NULL
## make date a real date:

wto1$date <- as.Date(wto1$date, format="%d%B%Y")

class(wto1$date)
## some descriptive statistics:

freq <- as.data.frame(table(wto1$country.speaker))
freqSpeakers <- as.data.frame(table(wto1$country.speaker))
colnames(freqSpeakers)

freqSpeakers <- freqSpeakers[order(freqSpeakers$Freq,
                                   decreasing=TRUE),]

freqSpeakers

## spot check some of the text:
rsubset <- sample(1:dim(wto1)[1], 5, replace=FALSE)
wto1$paratext[rsubset]

##Missing data?

sapply(wto1, function(x) sum(is.na(x)))


#### Do some text analysis:

wto1$numdate <- as.numeric(wto1$date)

metadatacols <- c("docid", "parnum", "country.speaker",
                "date", "numdate")

processed <- textProcessor(documents=wto1$paratext,
                           metadata=wto1[,metadatacols])

summary(processed) ##13,288 docs; 8667 words

out <- prepDocuments(processed$documents,
processed$vocab, processed$meta)

## 13284 docs, 5590 terms, 508643 tokens

docs <- out$docs
vocab <- out$vocab
meta <- out$meta

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
