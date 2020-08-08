### Preliminary analysis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

## After running search K in 50-100 range,
## K=60 and K=90 stood out as the two lowest residuals
## first analysis with K=60 for ease of human interpretation


rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 'pdftools',
           'tidyr', 'quanteda', "wbstats")

packs2 <- c("stringr", "reshape2",
            "dplyr", "ggplot2",  "magrittr")

loadPkg(packs)
loadPkg(packs2)

if(Sys.info()['user']=="Ergane"){ ## if on my own machine look in Dropbox
    dataPathDesktop <- "~/Dropbox/WTO/rdatas/"
}else{ ## else look in ~/WTO/
    dataPathDesktop <- "../../"
}


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

processed <- textProcessor(documents=speakers.meta$cleanedtext,
                           metadata=speakers.meta)

summary(processed) ## 8515 documents (paragraphs), 9570  word dictionary


out <- prepDocuments(processed$documents,
                     processed$vocab,
                     processed$meta) ## 8629 docums, 5856 terms

## Convert "date" field into a numeric counter:

sum(is.na(out$meta$date))

out$meta$numdate <- as.numeric(out$meta$date)

head(out$meta$numdate)
head(out$meta$date)

## rename objects for ease of reference:
docs <- out$documents
vocab <- out$vocab
meta <- out$meta


############################
#### Start running some models
###########################

set.seed(61920)


mod.select.60 <- stm::selectModel(documents=docs,
                               vocab=vocab,
                               data=meta,
                               K=60, ## 60 suggested by searchK
                               prevalence= ~ s(numdate) +
                               as.factor(income_level_iso3c),
                               seed=61920)


save.image(file=paste0(dataPathDesktop,
               "tradDevModSelect_60.RData"))
