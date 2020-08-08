
### Preliminary analysis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

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

packs3 <- c("textclean")
loadPkg(c(packs, packs2, packs3))

dataPathDesktop <-  "~/Dropbox/WTO/rdatas/"
dataPath <- "../../"
savePath <- "./"

## This has the WTO paragraph-level data,
## post-processing cleanup
## and wealth info

load(paste0(dataPathDesktop,"speakersMeta.Rdata"))

dim(speakers.meta)
colnames(speakers.meta)


## Verify no missing data in the columns we'll use:
sum(is.na(speakers.meta$docid))
sum(is.na(speakers.meta$country))
sum(is.na(speakers.meta$date))
sum(is.na(speakers.meta$paratext))
sum(is.na(speakers.meta$firstent))

####################################################
### Text cleanup
####################################################

speakers.meta$cleanedtext <- speakers.meta$paratext ## make fresh copy

## Ugly unicode:
speakers.meta$cleanedtext <- textclean::replace_non_ascii(speakers.meta$cleanedtext,
                                                          replacement=" ",
                                                          remove.nonconverted=TRUE)

## Unicode replacement character becomes "1/4." Dump:

speakers.meta$cleanedtext <- gsub(speakers.meta$cleanedtext,
                                  pattern="1\\/4",
                                  replace="")

## Hyphens are also a problem in the final outcome:

speakers.meta$cleanedtext <- gsub(speakers.meta$cleanedtext,## retain ecommerce
                                  pattern="e-commerce",
                                  replace="ecommerce")

speakers.meta$cleanedtext <- gsub(speakers.meta$cleanedtext,
                                  pattern="-",
                                  replace=" ")

## website words:

speakers.meta$cleanedtext <- gsub(speakers.meta$cleanedtext,
                                  pattern="www",
                                  replace="")

speakers.meta$cleanedtext <- gsub(speakers.meta$cleanedtext,
                                  pattern="http.",## http or https
                                  replace="")

## Remove extra whitespace
speakers.meta$cleanedtext <- textclean::replace_white(speakers.meta$cleanedtext)

save(speakers.meta,
           file=paste0(dataPathDesktop,"speakersMetaCleaned.Rdata"))

