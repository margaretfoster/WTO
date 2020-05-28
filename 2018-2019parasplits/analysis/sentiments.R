## This script...

rm(list=ls())


library(tokenizers)
library(tidytext)
library(sentimentr)
library(stringr)

dataPath <-  "../../data/"
resultsPath <- "../results/"

data <- read.csv(paste0(dataPath, "WTO_TD_NER_Data.csv"),
                 stringsAsFactors=FALSE)

class(data)
dim(data) ##7056 x 9
colnames(data) ##

data$X <- NULL

length(unique(data$docid)) ## should have 109
## 110 if there is M28A1 (appendix), can remove from analysis if want


### make a subset to work with:
## M25, since I happen to have that open on my computer
## and, as I look over it, M25 is scathing

m25 <- data[which(data$docid=="wtcomtdM25"),]

dim(m25) ## 180 x8
table(m25$firstent)

### take an even smaller subset:
## Mauritius, which agrees with Morocco (who seems to be the leader of the
## sckeptics)
tmp <- m25[which(m25$firstent=="Mauritius"),]

## first pargraph by Mauritius:
## expected sentiments:
## 1 => negative
## 2 => positive
## 3 -> neutral?
## 4-> negative
## 5 -> negative ('lack of encouraging response')

mytext <- get_sentences(tmp[1,"paratext"])


sentiment(mytext)

tst <- get_sentences(tmp)

class(tst)
head(tst)
dim(tst) ## 19 x10
colnames(tst)

dim(tmp) ## 4x8


## all sentences for meeting 25:
m25s <- get_sentences(m25)

## So what happens if I run "get sentences" for the whole document,
## delete the "ents" field (but keep the first ent as "speaker")
## and re-run this through the NER?

ls()
dim(data)

wto.sentences <- get_sentences(data)
dim(wto.sentences) ##58029 x10

colnames(wto.sentences)

colnames(wto.sentences)[which(colnames(wto.sentences)=="firstent")] <- "speaker"
colnames(wto.sentences)[which(colnames(wto.sentences)=="paratext")] <- "sentence"
colnames(wto.sentences)


## Pick back up here on 5/28:
## Want to match "ents" with the actual entities in the sentence:

tmp <- wto.sentences[1:100,] ## want to match e in ents in "sentence" and not in "speaker"

tmp$inline <- "none"

dim(tmp)## 11x11
head(tmp)

for(r in 1:dim(tmp)[1]){
    inl <- c()
-    print(r)
    if(is.na(tmp[r,]$ents==TRUE)){
        print("pass")
    }else{
        ## get list of found entities
        ids <- unlist(strsplit(tmp[r,]$ents, split=","))
    
        ## search those entities in the text:
        for(e in ids){
            #print(e)
            text <- tmp[r, "sentence"]
            speaker <- tmp[r,"speaker"]
            in.sen <- grepl(pattern=e, x=text)
            in.speaker <- grepl(pattern=e, x=speaker)
            ifelse(in.sen==TRUE && in.speaker==FALSE,
                   inl <- c(inl, e), print("."))
        }
        
        if(length(inl)>=1){
            tmp[r, "inline"] <- paste(paste0(inl, ","),
                                      sep=",",
                                      collapse=" ")
        }
    }
}

tmp[1:10, c('sentence',"inline")]

## remove all rows in "inline" that are just a space:


tmp$inline


tmp3 <- tmp[6:10,]

tmp3

trial <- c('test', 'assigment', 'of', 'vector')

        class(trial)




tmp3[5, "inline"] <- trial2

grepl(pattern=e, x=tmp)

print(tmp)

tmp[,"sentence"]

stringr::str_detect(tmp[,"sentence"], "Jamaca")



wto.sentences[1127,]
wto.sentences[7200,] ## this is just "mr", drop "sentences" less than a few words:


wto.sentences$phraselength <- stringr::str_count(wto.sentences$paratext, "\\w+")

summary(wto.sentences$phraselength)

wto.sentences[which(wto.sentences$phraselength==700),]

## TO DO AFTER WALK:
## for each entity in the ents column, search for the ent
## in the corresponding sentence (parse the references to only the sentences that contain the other state)


## table of sentence lenghts:

## Make a custom setiment dictionary:
## 5 point scale of words:


list.files(path=".", pattern="*.txt")

pos0 <- readLines(con="sentiments_neutral.txt") ##neutral
posp1 <- readLines(con="sentiments_somewhat_support.txt")
posn1 <- readLines(con="sentiments_somewhat_against.txt")
posp2 <- readLines(con="sentiments_strongly_support.txt")
posn2 <- readLines(con="sentiments_strongly_against.txt")

posn2
## want to build a relational network of references
## between the speaker and other ents

## something like:
## build the matrix of speaker locations in a text
## for each speaker, take the text betweeen the speaker
## and the next entity, and look for one of the words in our
## signals between the speaker and the next entity
## question 1: can I just look to the first entity? or should
## I bounce between speaker-other referenced entities dyads
## in order to to the speaker-first referenced entity dyad would want to take the text between the postion of the speaker and the position of the first non-speaker entity
## then for the speaker-subsequent entity dyads, want to take just the sentence surrounding the location of the nth referenced entity.

## for overall sentiment of the pargraph, can take the 5 pt scale phrases and assign -2, -1, 0, +1, +2 to each vector, then add up the numerical score of the paragraph?

## for overall sentiment of references, can do the same numerical scale, but only taking the sentence around the referenced entities. then build an adjacency matrix of sentiments.
