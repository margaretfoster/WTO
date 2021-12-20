## Measure 

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tidyr', 'quanteda', "classInt", "countrycode")
packs.quanteda <- c("quanteda.textmodels",
"quanteda.textplots")

loadPkg(c(packs, packs.quanteda))

#########################
## Declare Data Paths
#########################

if(Sys.info()['user']=="Ergane"){ ## if on my desktop
    dataPathDesktop <- "~/Dropbox/WTO-Data/rdatas/"
    print(paste0("The datapath is: ", dataPathDesktop))
}else{
    if(Sys.info()['user']=="Promachos"){ ## my laptop
        dataPathDesktop <- "~/Dropbox/WTO-Data/rdatas/"
        print(paste0("The datapath is: ", dataPathDesktop))
    }else{ ## else look in ~/WTO/
        dataPathDesktop <- "../../"
        print(paste0("The datapath is: ", dataPathDesktop))
    }
}

#############################
## Load Processed Data
#############################

load(paste0(dataPathDesktop,"processedTextforSTM.Rdata"))

############################
##### Analysis
############################

summary(out$meta$meetingno) ## up to M 113
summary(out$meta$year)

## Merge "ADMN" into "NOTST" in the metadata

out$meta$iso3c.il <- out$meta$income_level_iso3c

out$meta[which(out$meta$iso3c.il=="ADMN"), "iso3c.il"] <-"NOTST"

table(out$meta$income_level_iso3c)
table(out$meta$iso3c.il)

############################
## Subset to remove admin/non-state 

of.interest <- c("AGG","HIC","LIC","LMC","UMC")

dat2 <- out$meta

dat2 <- dat2[which(dat2$iso3c.il %in% of.interest),]
dim(out$meta) ## 8853
dim(dat2) ## 5229


### Convert dat2 into speaker-meeting
## from speaker-turn-meeting

library(dplyr)

dat3 <- dat2 %>% 
    group_by(meetingno, country) %>% 
    mutate(allcomments = paste0(cleanedtext, collapse = ""))

dat3$country_year <- paste0(dat3$country, "_", dat3$year)

docvars.list2 <- c("allcomments", "date",
                   "meetingno", "iso3c.il", "region",
                  "year", "country", "country_year")

dat3 <- unique(dat3[,docvars.list2])

dim(dat3) ## 2285 x 5

## add three-letter country code for plotting:
custom.match <- c("European Union" = "EU")
dat3$ccode <- countrycode(dat3$country,
                          origin='country.name',
                          destination='iso3c',
                          custom_match= custom.match)

table(dat3$ccode)

##speaker-meeting corpus
tst <- corpus(dat3$allcomments,
              docvars=dat3[,c("date", "country_year",
                  "meetingno", "iso3c.il",
                  "year", "country",
                  'ccode', "region")])

summary(tst)
print(tst)

### sample of meetings:
## M105;
## In terms of frustration words:
##2017-2018 are both the peak frustration years (~11% of words in the corpus for those years are "frustration" words; 2011-2012 are also high frustration years (9%) of words are frustration words

##(Better would be to go through each meeting and identify paragraphs that
## are most of each different frame (via the STM topics?) and then
## use the paragraph texts as the setpoints rather than a state ID)

## Fit meeting-level Wordscore Models 

require(quanteda.textmodels)

source("callWordscores.R")

## Meeting 05

tst.05 <- corpus_subset(tst, meetingno==05)

out05 <- callWS(corpus.subset=tst.05,
                fixed.point1="India",
                fixed.point2="United States")

pdf(file="wordscoresM05.pdf")
textplot_scale1d(out05$wordscores,
                 doclabels=docvars(tst.05, "country_year"))

dev.off()

## Meeting 15

tst.15 <- corpus_subset(tst, meetingno==15)

out15 <- callWS(corpus.subset=tst.15,
                fixed.point1="India",
                fixed.point2="United States")

pdf(file="wordscoresM15.pdf")
textplot_scale1d(out15$wordscores,
                 doclabels=docvars(tst.15, "country_year"))
 ##(note-- tried grouping by income and region, neither was that informative))
dev.off()

## Meeting 25
tst.25 <- corpus_subset(tst, meetingno==25)

out25 <- callWS(corpus.subset=tst.25,
                fixed.point1="India",
                fixed.point2="United States")

pdf(file="wordscoresM25.pdf")
textplot_scale1d(out25$wordscores,
                 doclabels=docvars(tst.25, "country_year"))
dev.off()

## Meeting 45
tst.45 <- corpus_subset(tst, meetingno==45)

tst.45 <- corpus_subset(tst, meetingno==45)

out45 <- callWS(corpus.subset=tst.45,
                fixed.point1="India",
                fixed.point2="United States")

##pdf(file="wordscoresM45.pdf")
textplot_scale1d(out45$wordscores,
                 doclabels=docvars(tst.45, "country_year"))
 ##(note-- tried grouping by income and region, neither was that informative))

dev.off()

## Meeting 75
tst.75 <- corpus_subset(tst, meetingno==75)

out75 <- callWS(corpus.subset=tst.75,
                fixed.point1="India",
                fixed.point2="United States")

pdf(file="wordscoresM75.pdf")
textplot_scale1d(out75$wordscores,
                 doclabels=docvars(tst.75, "country_year"))
dev.off()


## Meeting 105 (very contentious)
tst.105 <- corpus_subset(tst, meetingno==105)

out105 <- callWS(corpus.subset=tst.105,
                fixed.point1="India",
                fixed.point2="United States")

pdf(file="wordscoresM105.pdf")

textplot_scale1d(out105$wordscores,
                 doclabels=docvars(tst.105, "country_year"))
dev.off()


## Meeting 110 
tst.110 <- corpus_subset(tst, meetingno==110)

out110 <- callWS(corpus.subset=tst.110,
                fixed.point1="India",
                fixed.point2="United States")

pdf(file="wordscoresM110.pdf")

textplot_scale1d(out110$wordscores,
                 doclabels=docvars(tst.110, "country_year"))
dev.off()


## Meeting 113 (Pandemic era)
tst.113 <- corpus_subset(tst, meetingno==113)

out113 <- callWS(corpus.subset=tst.113,
                fixed.point1="India",
                fixed.point2="United States")

pdf(file="wordscoresM113.pdf")
textplot_scale1d(out113$wordscores,
                 doclabels=docvars(tst.113, "country_year"))

dev.off()
