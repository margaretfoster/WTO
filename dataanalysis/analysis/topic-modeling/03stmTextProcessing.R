
## Process into the STM format.
## separate central script to customize
## stopwords, stemming
## and have that percolate through rest of analyis

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 
           'tidyr', 'quanteda')

packs2 <- c("stringr", "reshape2",
            "dplyr", "ggplot2",  "magrittr")

packs3 <- c("textclean")
loadPkg(c(packs, packs2, packs3))

## dataPathDesktop <-  "~/Dropbox/WTO/rdatas/"
dataPath <- "../../../dataprepandclean/"
savePath <- "./"

## This has the WTO paragraph-level data,
## Meetings 01-113
## post-processing cleanup
## and wealth info
##

speakers.meta <- read.csv(paste0(dataPath,"WTOSpeakerTurnsM1to113.csv"),
                          stringsAsFactors=FALSE)

summary(speakers.meta)

dim(speakers.meta) ##8855 x 15

### Format variables into correct classes
speakers.meta$date <- as.Date(speakers.meta$date,
                              format="%Y-%m-%d")

summary(speakers.meta$date)

## Verify no missing data in the columns we'll use:
sum(is.na(speakers.meta$docid))
sum(is.na(speakers.meta$country))
sum(is.na(speakers.meta$date))
sum(is.na(speakers.meta$cleanedtext))
sum(is.na(speakers.meta$firstent))

## The one NA is an empty row:
speakers.meta <- na.omit(speakers.meta)


### Add numdate for the STM model
speakers.meta$numdate <- as.numeric(speakers.meta$date)

dim(speakers.meta)


#################
##### Cleanup for STM
##################

pageMarkupStopWords <- c("hyperref", "toc", "wtcomtdw",
                         "pageref") ## not meeting content

processed <- textProcessor(documents=speakers.meta$cleanedtext,
                           metadata=speakers.meta,
                           removenumbers=TRUE, 
                           customstopwords=pageMarkupStopWords)

summary(processed) ## 8853 documents (paragraphs), 7109  word dictionary

out <- prepDocuments(processed$documents,
                     processed$vocab,
                     processed$meta)


## rename objects for ease of reference:
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

ls()

save(out, docs, vocab, meta,
     file=paste0(savePath, "processedTextforSTM.RData"))
 
