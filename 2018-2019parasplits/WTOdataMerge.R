## script to merge in WTO NER, speaker, and paragraph data

## read in data csvs
## merge according to meeting number and paragraph

dat1 <- read.csv("wtoSecondStageDataParaNum.csv",
                 stringsAsFactors=FALSE)

dat2 <- read.csv("NERwithparanums.csv",
                 stringsAsFactors=FALSE,
                 header=FALSE)

colnames(dat2) <- c("row", "docid", "key", "entities")


ls()

dim(dat1)
dim(dat2)

colnames(dat1)
colnames(dat2)

head(dat2$paraid)
head(dat1$key)

wto

## Read in speakers:

library(stringr)


tst <- head(dat2$entities)

dat2$speaker <- word(dat2$entities, 1)

head(dat2)




dat2[379,]$entities
cbind(dat2$speaker, dat2$entities)

## "united" => "United States"
## El => El Salividor
## Hong => Hong Kong
## Costa=> Costa Rica
## New => New Zealand
## Sri -> Sri Lanka

## Find the "South" and hand-correct those

unique(dat2$speaker)
