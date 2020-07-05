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

dataPath <- "../../"
savePath <- "./"

## This has the WTO paragraph-level data,
## post-processing cleanup
## and wealth info

load("speakersMeta.Rdata")

colnames(speakers.meta)

dim(speakers.meta)

sum(is.na(speakers.meta$docid))
sum(is.na(speakers.meta$country))
sum(is.na(speakers.meta$date))
sum(is.na(speakers.meta$paratext))
sum(is.na(speakers.meta$firstent))

####################
## Summary stats on overall speaker frequency:
###################

speakers.meta$firstent <- as.factor(speakers.meta$firstent)

length(unique(speakers.meta$firstent)) ##now 167

### Summary statistics:
speaker.freq <- as.data.frame((table(speakers.meta$firstent)))

speaker.freq <- speaker.freq[order(speaker.freq$Freq),]

head(speaker.freq)

## 10 most frequent speakers:

speaker.freq[157:167,]

## Chairman/person; Committee
## Then: US,EU/EC, India, Egypt,  Canada, Seretariat,
## China, Japan, Switzerland

## 10 most common speaking countries:

speaker.freq[154:167,]

## US, India, EU/EC, Egypt, Canada,
## China, Japan, Switzerland, Brazil, Bangladesh,
## US by far the most common speaker (600+);
## EC/EU with 479 combined between EU and EC;
## Indian at 338; then a cluster in the 200-300.
## Then another cluster in the 150 range.

## 7/4/20: Went in by hand to the NER data to
## fix 250 "empty" speakers. Some missed countries, some
## NER confused by extra commas. Vast majority of the missing
## speakers were the chairman speaking, but NER missed several:
## El Salvador, Cote d'Ivoire, Saudi Arabia, and Russian Federation

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
speaker.year[which(speaker.year$firstent=="Egypt"),] 


#################
##### Analysis
##################

processed <- textProcessor(documents=speakers.meta$paratext,
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

mod.out.100 <- stm(documents=docs,
               vocab=vocab,
               data=meta,
               K=100, ## start at 100 to get a fine-grained
               ## overview over time
               prevalence= ~ s(numdate) +
                   as.factor(docid)+ 
                   as.factor(incomeID), ## speaker-level is ~175 speakers(!)
               seed=61920)

 

prep.100 <- estimateEffect(c(1:100)~ s(numdate) +
                           as.factor(docid)+
                           as.factor(firstent),
                       mod.out.100,
                       metadata=meta,
                       documents=docs,
                       uncertainty=c("Global"))

### K=75

mod.out.75 <- stm(documents=docs,
               vocab=vocab,
               data=meta,
               K=75, ## step down to 75
               prevalence= ~ s(numdate) +
                   as.factor(docid)+
                   as.factor(incomeID),
               seed=61920)



prep.75 <- estimateEffect(c(1:75)~ s(numdate) +
                           as.factor(docid)+
                           as.factor(incomeID),
                       mod.out,
                       metadata=meta,
                       documents=docs,
                       uncertainty=c("Global"))

save.image(file="tradDevPara75_100.RData")
