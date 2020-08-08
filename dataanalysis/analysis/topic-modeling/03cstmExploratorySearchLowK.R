
### Preliminary analysis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

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


processed <- textProcessor(documents=speakers.meta$cleanedtext,
                           metadata=speakers.meta)

summary(processed) ## 8515 documents (paragraphs), 7093  word dictionary

out <- prepDocuments(processed$documents,
                     processed$vocab,
                     processed$meta)

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
#### Search K over large number of potential topics
###########################

set.seed(80620)

### searchk

Ks=seq(from=10, to=50,by=10)

mod.tdsk <- searchK(documents=docs,
                       vocab=vocab,
                       K=Ks,
                    prevalence=~s(numdate)+
                    as.factor(income_level_iso3c),
                       data=meta,
                    seed=80620)

save(mod.tdsk,
     file=paste0(dataPathDesktop, "sK10to50.Rdata"))
 
