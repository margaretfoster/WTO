## This script...

rm(list=ls())


library(tokenizers)
library(tidytext)
library(sentimentr)
library(stringr)

## Data:
args = commandArgs(trailingOnly=TRUE)

infile <- args[1] ## input data
outfile <- args[2] ## name of file for ouput


data <- read.csv(infile,
                 stringsAsFactors=FALSE)


dim(data) ##8544 x 9


wto.sentences <- get_sentences(data)
dim(wto.sentences) ##58029 x10


## Make column names accurate:
colnames(wto.sentences)[which(colnames(wto.sentences)=="firstent")] <- "speaker"
colnames(wto.sentences)[which(colnames(wto.sentences)=="paratext")] <- "sentence"
colnames(wto.sentences)

## for each entity in the ents column, search for the ent
## in the corresponding sentence (parse the references to only the sentences that contain the other state)

wto.sentences$inline <- "none"

dim(wto.sentences)## 11x11
head(wto.sentences)

for(r in 1:dim(wto.sentences)[1]){
    inl <- c()
    print(r)
    if(is.na(wto.sentences[r,]$ents==TRUE)){
        print("pass")
    }else{
        ## get list of found entities
        ids <- unlist(strsplit(wto.sentences[r,]$ents, split=","))
    
        ## search those entities in the text:
        for(e in ids){
            #print(e)
            text <- wto.sentences[r, "sentence"]
            speaker <- wto.sentences[r,"speaker"]
            in.sen <- grepl(pattern=e, x=text)
            in.speaker <- grepl(pattern=e, x=speaker)
            ifelse(in.sen==TRUE && in.speaker==FALSE,
                   inl <- c(inl, e), print("."))
        }
        
        if(length(inl)>=1){
            wto.sentences[r, "inline"] <- paste(paste0(inl, ","),
                                      sep=",",
                                      collapse=" ")
        }
    }
}


write.csv(wto.sentences,
          file=outfile)
