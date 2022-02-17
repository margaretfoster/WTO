### Preliminary analysis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

## This specification: Model covariates by year with faction
## content covariates

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

load(paste0(dataPathDesktop,
            "processedTextforSTMDeDeup.RData"))

############################
##### Analysis
############################

############################
## Inductive Search for Underlying dimensions of discussion
## & Which delegations are most associated with those
## topics in each meeting
############################
## Identify the shocks
## Pre/post China

out$meta$chinajoined <- ifelse(out$meta$year <= 2001,
                               0, 1)
summary(out$meta[which(
    out$meta$chinajoined==0),]$meetingno)
summary(out$meta[which(
    out$meta$chinajoined==1),]$year)

## Short-term shock:
out$meta$chinashock <- ifelse(out$meta$year == 2002 |
                               out$meta$year== 2003,
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
out$meta$Xishock <- ifelse(out$meta$year == 2013 |
                               out$meta$year== 2014,
                               1, 0) 
summary(out$meta[which(
    out$meta$Xishock==0),]$meetingno)
summary(out$meta[which(
    out$meta$Xishock==1),]$year)

table(out$meta[which(out$meta$Xishock==1),]$year) ## 2013-2014

## Pre-Post Trump/right-wing populists

out$meta$postTrump <- ifelse(out$meta$year <= 2016,
                             0, 1)


## Short-term shock:
out$meta$Trumpshock <- ifelse(out$meta$year == 2017 |
                               out$meta$year== 2018,
                               1, 0) 
summary(out$meta[which(
    out$meta$Trumpshock==0),]$meetingno)
summary(out$meta[which(
    out$meta$Trumpshock==1),]$year)

table(out$meta[which(
    out$meta$Trumpshock==1),]$year) ## 2017-2018

## Covid-19 Pandemic
out$meta$postcovid <- ifelse(out$meta$year <= 2020,
                             0, 1)


## Short-Term Shock:
out$meta$covidshock <- ifelse(out$meta$year == 2020 |
                               out$meta$year== 2021,
                               1, 0) 
summary(out$meta[which(
    out$meta$covidshock==0),]$meetingno)
summary(out$meta[which(
    out$meta$covidshock==1),]$year)

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


### Remove numbers, punctuation, special characters:
library(quanteda)

states.subset$cleanedtext <- gsub("[[:digit:]]", "",
                                  states.subset$cleanedtext) 

states.subset$cleanedtext <- gsub("[[:punct:]]", "",
                                  states.subset$cleanedtext) 


############################
## Prepare data
data <- corpus(states.subset, text_field = 'cleanedtext')
docvars(data)$text <- as.character(data)

data <- dfm(data,
            stem = TRUE,
            remove = c(stopwords('english'),
                "wtcom*", "said"),
            remove_symbols = TRUE,
            split_hyphens = TRUE) %>%
    dfm_trim(min_termfreq = 2)

out <- convert(data, to = 'stm')

## Add faction/category metadata:

#### Model sub-themes

## For identification of redistributors and
## reciprocators, see draft appendix

recip <- c("European Union", "Canada",
           "Japan", "Switzerland",
           "Trinidad and Tobago",
           "United States")

redist <- c("Argentina","Bangladesh", "Bolivia",
            "Brazil", "Cambodia", 
            "China", "Cote d'Ivoire",
            "Cuba", "Dominca", 
            "Egypt", "El Salvador",
            "Fiji", "Guatemala",
            "India", "Jamaica",
            "Kenya", "Lesotho", "Malaysia",
            "Mexico", "Morocco",
            "Poland", "Sri Lanka",
            "Uganda", "Uruguay",
            "Zambia")


## Also more stripped-down factions:

faction1 <- c("European Union", "Canada",
              "United States")

faction2 <- c("China", "Egypt", "India")

##Faction
out$meta$faction <- "Other"
out$meta[which(
    out$meta$firstent %in% faction1),
         "faction"] <- "US-EU-Can"

out$meta[which(
    out$meta$firstent %in% faction2),
         "faction"] <- "China-Egypt-India"

## redist-recipro
out$meta$cat <- "Other"
out$meta[which(
    out$meta$firstent %in% recip),
         "cat"] <- "Reciprocators"

out$meta[which(
    out$meta$firstent %in% redist),
         "cat"] <- "Redistributors"


out$meta$faction <- as.factor(out$meta$faction)
out$meta$cat <- as.factor(out$meta$cat)

#####################
## K-2 Overview Model:
#####################

base.model = ~s(year)
faction.model= ~s(year)+ faction
category.model= ~s(year)+ cat

## Category Model:
mod.out.2 <- stm(documents=out$documents,
                     vocab=out$vocab,
                     data=out$meta,
                     K=2, ## 
                     prevalence=base.model,
                     seed=61921)

prep.2 <- estimateEffect(c(1:2) ~s(year),
                             mod.out.2,
                             metadata=out$meta,
                             documents=out$documents,
                             uncertainty=c("Global"))

## ugly patch while I decide if the year-only specification is what I want
## to work with:

mod.out.2.cat <- mod.out.2
mod.out.2.fac <- mod.out.2
prep.2.cat <- prep.2
prep.2.fac <- prep.2

save.image(file="twoTopicsAndSubSets-NoAdminSubset_CatFacRepl.Rdata")
