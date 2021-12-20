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

loadPkg(packs)


#########################
## Declare Data Paths
#########################

if(Sys.info()['user']=="Ergane" |
   Sys.info()['user']=="Promachos"){ ## if on my own machines look in Dropbox
    print(Sys.info()['user'])
    dataPathDesktop <- "~/Dropbox/WTO-Data/rdatas/"
    print(paste0("The datapath is: ", dataPathDesktop))
}else{ ## else look in ~/WTO/
    dataPathDesktop <- "../../"
    print(paste0("The datapath is: ", dataPathDesktop))
}

#############################
## Load Processed Data
#############################

load(paste0(dataPathDesktop,"processedTextforSTM.Rdata"))

############################
##### Analysis
############################

############################
## Inductive Search for Underlying dimensions of discussion
## And, per-meeting, which delegations are most associated with those
## topics
######################
## Add in Shock Time Periods

############################
## Identifying the different shocks
##
colnames(out$meta)

length(docs)

## Pre/post China

out$meta$chinajoined <- ifelse(out$meta$year <= 2001,
                               0, 1)
summary(out$meta[which(
    out$meta$chinajoined==0),]$meetingno)
summary(out$meta[which(
    out$meta$chinajoined==1),]$year)

## Short-term shock:
out$meta$chinashock <- ifelse(out$meta$year == 2001 |
                               out$meta$year== 2002,
                               1, 0)
summary(out$meta[which(
    out$meta$chinashock==0),]$meetingno)
summary(out$meta[which(
    out$meta$chinashock==1),]$year)

table(out$meta[which(out$meta$chinashock==1),]$year) ## 2001-2002

## Pre/Post Financial Crisis
##Note 2009 b/c FC was at end of 2008
out$meta$postFC <- ifelse(out$meta$year <= 2009, 0, 1)

summary(out$meta[which(
    out$meta$postFC==0),]$meetingno)
summary(out$meta[which(
    out$meta$postFC==1),]$year)

## Short-term shock:
out$meta$FCshock <- ifelse(out$meta$year == 2009 |
                               out$meta$year== 2010,
                               1, 0) 
summary(out$meta[which(
    out$meta$FCshock==0),]$meetingno)
summary(out$meta[which(
    out$meta$FCshock==1),]$year)

table(out$meta[which(out$meta$FCshock==1),]$year) ## 2009-2010
## 72 meetings before 2008 Financial Crisis,2009-2020

## Pre-Post Xi

out$meta$postXi <- ifelse(out$meta$year <= 2012,
                          0, 1)
table(out$meta$postXi)

## Short-term shock:
out$meta$Xishock <- ifelse(out$meta$year == 2012 |
                               out$meta$year== 2013,
                               1, 0) 
summary(out$meta[which(
    out$meta$Xishock==0),]$meetingno)
summary(out$meta[which(
    out$meta$Xishock==1),]$year)

table(out$meta[which(out$meta$Xishock==1),]$year) ## 2012-2013

## Pre-Post Trump/right-wing populists

out$meta$postTrump <- ifelse(out$meta$year <= 2016,
                             0, 1)


## Short-term shock:
out$meta$Trumpshock <- ifelse(out$meta$year == 2016 |
                               out$meta$year== 2017,
                               1, 0) 
summary(out$meta[which(
    out$meta$Trumpshock==0),]$meetingno)
summary(out$meta[which(
    out$meta$Trumpshock==1),]$year)

table(out$meta[which(out$meta$Trumpshock==1),]$year) ## 2012-201
######################
## Subset data to omit non-state speakers
## (Here as all actors not associated with a specific geography)
geos <- c("Aggregates", "East Asia & Pacific",
          "Europe & Central Asia",
          "Latin America & Caribbean",
          "Middle East & North Africa", "North America",
          "South Asia", "Sub-Saharan Africa")


## Reprocess into a subset that is just state speakers:

states.subset <- out$meta[which(out$meta$region %in% geos),]

dim(out$meta) ## 8853 x 20
dim(states.subset) ## 5229 x 20

library(quanteda)

### Remove numbers, punctuation, special characters:

colnames(states.subset)

states.subset$cleanedtext <- gsub("[[:digit:]]", "",
                                  states.subset$cleanedtext) 

states.subset$cleanedtext <- gsub("[[:punct:]]", "",
                                  states.subset$cleanedtext) 

##states.subset$cleanedtext <- tolower(states.subset$cleanedtext) ## idk why but making everything lowercase conflates the two topics

head(states.subset$cleanedtext)
tail(states.subset$cleanedtext)


# prepare data
data <- corpus(states.subset, text_field = 'cleanedtext')
docvars(data)$text <- as.character(data)

data <- dfm(data, stem = TRUE,
            remove = c(stopwords('english'),
                "wtcom*", "said"),
            remove_symbols = TRUE,
            split_hyphens = TRUE) %>%
    dfm_trim(min_termfreq = 2)
out <- convert(data, to = 'stm')

form= ~s(numdate)

mod.out.2 <- stm(documents=out$documents,
                  vocab=out$vocab,
                  data=out$meta,
                 K=2, ## 
                  prevalence=form ,
               seed=61921)
 

prep.2 <- estimateEffect(c(1:2) ~s(numdate),
                         mod.out.2,
                          metadata=out$meta,
                          documents=out$documents,
                          uncertainty=c("Global"))

summary(mod.out.2) ## Topic 1: notification, schemes, ratifications
## Topic two: development frame

save.image(file="twoTopicsAndSubSets-NoAdminSubset.Rdata")
