
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

dataPathDesktop <-  "~/Dropbox/WTO/rdatas/"
dataPath <- "../../"
savePath <- "./"

## This has the WTO paragraph-level data,
## post-processing cleanup
## and wealth info

load(paste0(dataPathDesktop,"speakersMetaCleaned.Rdata"))

colnames(speakers.meta)

dim(speakers.meta)

## Verify no missing data in the columns we'll use:
sum(is.na(speakers.meta$docid))
sum(is.na(speakers.meta$country))
sum(is.na(speakers.meta$date))
sum(is.na(speakers.meta$cleanedtext))
sum(is.na(speakers.meta$firstent))

#################
##### Analysis
##################

pageMarkupStopWords <- c("hyperref", "toc", "wtcomtdw",
                         "pageref") ## have nothing to do with meeting content

processed <- textProcessor(documents=speakers.meta$cleanedtext,
                           metadata=speakers.meta,
                           removenumbers=TRUE, 
                           customstopwords=pageMarkupStopWords)

summary(processed) ## 8515 documents (paragraphs), 7093  word dictionary

out <- prepDocuments(processed$documents,
                     processed$vocab,
                     processed$meta)

## Convert "date" field into a numeric counter:

out$meta$numdate <- as.numeric(out$meta$date)

## rename objects for ease of reference:
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

ls()

############################
#### Search K over large number of potential topics
###########################

save(out, docs, vocab, meta,
     file=paste0(dataPathDesktop, "processedTextforSTM.RData"))
 
