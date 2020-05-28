#!/usr/bin/env R

rm(list=ls())

library(tidyverse)

## This script takes the output of adaptedNER
## and the list of identified entities

## (0) Reads in the data
##  (1) Takes the NLTK- idenfieid list of entities
## (2) Looks for a couple of entities that fell through the
## cracks in the python code
## (3)  matches the position of the entities in the paratext document
## (4)  finds the minimum
## (5) writes that as the first entity, which is assumed
## to be the speaker

## Load data:

args = commandArgs(trailingOnly=TRUE)
outfile <- args[2] ## name of file for ouput

## Read in data:
##docs = read.csv(args[1], ## documents with NER identified
##                header=TRUE,
##                stringsAsFactors=FALSE)

docs <- read.csv(args[1])


docs$paratext <- as.character(docs$paratext)
docs$ents <- as.character(docs$ents)

## clean up the "\n" literals:
docs$paratext <- gsub(docs$paratext,
     pattern="\n",
     replacement=" ")

##Some ugly corner case correction:
## "Secretariats" (row 1085) seems to be the only place where
## a plural is messing this up:
## other soultions have been more ugly

docs$paratext <- gsub(docs$paratext,
     pattern="Secretariats", ## really for row 1085
     replacement="Secretariat")
 

## Add a place for the new data to live:

docs$firstent <- NA

## Add a counter:
rows <- 1:dim(docs)[1] ## number of rows 

## "Overloooked" entities:
## (For some reason, NLTK missed a lot of
## entries for "Chairman", "Secretariat", and "Uganda")
## Even though they were in the entities dictionary


overlooked <- c("Chairman", "Secretariat",
                "Uganda")


## (1) for each entry in "ents
## (2) look for known-missing entities
## (3)find position in "paratext"
## take the minimum
## write that as "speaker"

## Add the missing Chairman and Secretariat lists:

print("Adding missing entities")

for (r in rows){
    ##   print(r)
    for(o in overlooked){
        ## see if the text has one of the missing values
        intext <- stringr::str_detect(
                                string=docs[r, "paratext"],
                               pattern = paste0("\\b",
                                                o, "\\b"))
                               ## \\b for word boundary
        ## and if the value is in the nltk list of entities:
        inents <- stringr::str_detect(
                               string=docs[r, "ents"],
                               pattern = o)
        
        ## note the rows and entities added:
        if(intext==TRUE & inents==FALSE){
            ## Make a note:
            sink(file="nltkmissedents.txt",
            append=TRUE)
            print(c(o, r))## print entity and row
            sink()
            ## Now fix:
            newents <- paste0(docs[r, "ents"],", ", o)
            docs[r, "ents"] <-  newents
        }
    } ## close "overlooked" for-loop
} ## close "rows" for-loop


## Now ID the first:
for (r in rows){
    print(r)
    ents <- trimws(stringr::str_split(docs[r,"ents"],
                                      pattern= ",",
                                      simplify=TRUE))
    
    ##create a data structure to identify the
    ## entities and their position in the text
    storage <- data.frame(matrix(NA, nrow=2,
                                 ncol=length(ents)),
                          row.names=c("start", "end"))
    
    colnames(storage) <- as.vector(ents)
    
    ## Go through the list of entities
    ## and identity their places in the
    ## paragraph text:
    for (e in 1:length(ents)){
        tmp <- paste0("\\b", ents[e], "\\b") ## \\b are word boundaries
        ##print(tmp)
        locs <- stringr::str_locate(
            string=docs[r, "paratext"],
            pattern=tmp)
        storage["start", e] <- locs[1]
        storage["end", e] <- locs[2]
    }
    
    ## Identify the minium of the
    ## numbers.
    minnum <- min(storage["start",])
   
    ## Find the column name
    ## where the minimum lives:
    id <- names(storage)[storage[1,] == minnum]
    ## Feed that back into the data:
    docs[r,]$firstent <- id    
}


write.csv(docs,
          file=outfile)

print("csv written")
