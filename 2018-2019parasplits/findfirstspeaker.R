#!/usr/bin/env R

##install.packages("tidyverse")

library(tidyverse)
## send in the documents:

args = commandArgs(trailingOnly=TRUE)
docs = read.csv(args[1]) ## documents with NER identified

## For development purposes:


## This takes the output of adaptedNER
## and the list of identified entities

## reads in the NER idenfieid output
## matches the position of the entities in the paratext document
## finds the minimum
## writes that as "speaker"

## TODO 4/9: see if this scales up to more than one
## column

## Load NER Data
tempdocs <- read.csv("testout.csv",
                     header=TRUE,
                     stringsAsFactors=FALSE)

colnames(tempdocs)

tempdocs$paratext <- as.character(tempdocs$paratext)
tempdocs$ents <- as.character(tempdocs$ents)


## now, for each entry in "ents"
## find position in "paratext"
## take the minimum
## write that as "speaker"

## add rows:


rows <- 20:30
minsdf <- data.frame()

#for (r in rows){
    ents <- trimws(stringr::str_split(tempdocs[r,"ents"],
                                      pattern= ",",
                                      simplify=TRUE))
    
    ents
    
    storage <- data.frame(matrix(NA, nrow=2, ncol=length(ents)),
                          row.names=c("start", "end"))
    ##df b/c mixed data types
    
    colnames(storage) <- as.vector(ents)
    
    for (e in 1:length(ents)){
        
        print(e)
        locs <- str_locate(
            string=tempdocs[15, "paratext"],
            pattern=ents[e])
        storage["start", e] <- locs[1]
        storage["end", e] <- locs[2]
    }
    
    minnum <- min(storage["start",])
    
    
    ## Find the column name
    ## with the specified value:
    id <- names(storage)[storage[1,] == minnum]

    group <- c(id, minnum)
    print(group)
    minsdf <- rbind(minsdf, group)
    
 #   }

#minsdf
