## This script runs paragraph-level sentiment analysis
## on the WTO TD Meeting minutes (disaggregated at the
## sentence level

rm(list=ls())


library(tokenizers)
library(tidytext)
library(sentimentr)
library(stringr)

dataPath <-  "../../data/"
resultsPath <- "../results/"

data <- read.csv(paste0(dataPath, "WTO_TD_NER_Sentences.csv"),
                 stringsAsFactors=FALSE)

class(data)
dim(data) ##8644  x 13
length(unique(data$docid)) ## 109
colnames(data) ##

data$X <- NULL

## how many dates am I missing?

data$date <- as.Date(data$date)
summary(data$date) ## No NA in the list


### make a subset to work with:
## M25, since I happen to have that open on my computer
## and, as I look over it, M25 is scathing

m25 <- data[which(data$docid=="WTCOMTDM25"),]

class(m25)

m25s <- get
dim(m25) ##1209 x 11


m25sentiments <- sentiment(m25[,'sentence'])
dim(m25sentiments)

colnames(m25sentiments)

colnames(m25)

m25sentiments[50:60,]

m25[54:55,c("speaker", "sentence",
            'sentence_id', 'element_id')]


m25s <- merge(x=m25,
              y=m25sentiments,
              by.x=c("sentence_id",
                     "element_id"),
              by.y=c("sentence_id",
                     "element_id"))

dim(m25s)

### take an even smaller subset:
## Mauritius, which agrees with Morocco (who seems to be the leader of the
## sckeptics)

colnames(m25)

tmp <- m25[which(m25$speaker=="Mauritius"),]

dim(tmp) ## 19x 11

## first pargraph by Mauritius:
## expected sentiments:
## 1 => negative
## 2 => positive
## 3 -> neutral?
## 4-> negative
## 5 -> negative ('lack of encouraging respones')

colnames(tmp)

mytext <- sentiment(tmp[,"sentence"])

class(mytext)
head(mytext)

## this pulls up ".3" for the sentiment analysis
## this is a relatively positive sentence

tmp[6, "sentence"]
## evaluate:
## sentence 1: thanking EU, moderate positive
## sentence 2:announcing dislike of the questionnaire
## sentence 3: strong support of Morocco
## sentence 4: mostly supporting Eygpt
## sentence 5: "implementation issues"
## sentence 6: 

## Make a custom setiment dictionary:
## 5 point scale of words:

list.files(path=".", pattern="*.txt")

pos0 <- readLines(con="sentiments_neutral.txt") ##neutral
posp1 <- readLines(con="sentiments_somewhat_support.txt")
posn1 <- readLines(con="sentiments_somewhat_against.txt")
posp2 <- readLines(con="sentiments_strongly_support.txt")
posn2 <- readLines(con="sentiments_strongly_against.txt")

posn2

