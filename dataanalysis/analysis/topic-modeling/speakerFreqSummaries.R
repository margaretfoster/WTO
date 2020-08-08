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

loadPkg(packs)
loadPkg(packs2)

dataPathDesktop <- "~/Dropbox/WTO/rdatas/"


dataPath <- "../../"
savePath <- "./"

## This has the WTO paragraph-level data,
## post-processing cleanup
## and wealth info

load(paste0(dataPathDesktop,"speakersMetaCleaned.Rdata"))

colnames(speakers.meta)

dim(speakers.meta)

## Verify no missing data in the columns we'll use:
sum(is.na(speakers.meta$firstent))

####################
## Summary stats on overall speaker frequency:
###################

speakers.meta$firstent <- as.factor(speakers.meta$firstent)

length(unique(speakers.meta$firstent)) #197

### Summary statistics:
speaker.freq <- as.data.frame((table(speakers.meta$firstent)))
speaker.freq <- speaker.freq[order(speaker.freq$Freq),]
head(speaker.freq)

## 10 most frequent speakers:

speaker.freq[187:197,]

## Chairman/person; Committee
## Then: US,EU/EC, India, Egypt,  Canada, Seretariat,
## China, Japan, Switzerland


##############################################
## Dataframe
## that is country-speaking totals across time:
## (years are more tractible to visualize than meeting minutes)
##############################################

speaker.year <- speakers.meta%>%
    group_by(firstent, year) %>%
    summarise( n = n())

head(speaker.year)
dim(speaker.year) ## 1260 x3
speaker.year$year <- as.numeric(speaker.year$year)


## Morocco is super interesting:
## two big spikes of activity in 1997, 1998, 2003: 
speaker.year[which(speaker.year$firstent=="Morocco"),]

## India: unusual[ish] spike in 2002:
speaker.year[which(speaker.year$firstent=="India"),] 

## Ugana: Ebb and flow form 1996-2017
speaker.year[which(speaker.year$firstent=="Uganda"),] 

## Egypt: big dropoff after 2001

print(speaker.year[which(speaker.year$firstent=="Egypt"),],
      n=24)
