###code to centralize the preparation of the STM
## out dataframe

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

## ensure date a real date:
wto1$date <- as.Date(wto1$date, format="%d%B%Y")
class(wto1$date)


wto1$numdate <- as.numeric(wto1$date)

## some descriptive statistics:

## spot check some of the text:
rsubset <- sample(1:dim(wto1)[1], 5, replace=FALSE)
wto1$paratext[rsubset]

##Missing data?

sapply(wto1, function(x) sum(is.na(x)))

#### prep for text analysis:

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

## see if any missing data:
sapply(out$meta, length) ## all 13284

## save
save(out, file="tradeandDevSTMOut.Rdata")

rm(out)
############################################
## subset wto1 to have only paragraphs of a
## certain # of words
## (to remove preambles, non-informative paragraphs)
############################################

wto1$paratext[1:10]

## length of paragraph wanted:

len <- 50 ## 50 words or more

## code that will count number of words:

cutoff <- which(sapply(strsplit(wto1$paratext, " "), length) > len)

## 8106 paragraphs in entire data:

length(cutoff)

wto2 <- wto1[cutoff,]

dim(wto2)


processed <- textProcessor(documents=wto2$paratext,
                           metadata=wto2[,metadatacols])

summary(processed) ##13,288 docs; 8667 words

outcens <- prepDocuments(processed$documents,
processed$vocab, processed$meta)

## 8104 docs, 5419 terms

docs <- outcens$docs
vocab <- outcens$vocab
meta <- outcens$meta


save(outcens, file="tradeandDevSTMOut50Plus.Rdata")
