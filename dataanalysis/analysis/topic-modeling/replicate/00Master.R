## 01dupAnalysisDataPrep.R
rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in%
           installed.packages()[,1])
            {install.packages(lib,
                              repos='http://cran.rstudio.com/')}
        suppressMessages( library(lib,
                                  character.only=TRUE) ) }}

packs <- c('tm', 'stm', 'readxl',
           'tidyr', 'quanteda')

packs2 <- c("stringr", "reshape2",
            "dplyr")

packs3 <- c("textclean", "textreuse")

loadPkg(c(packs, packs2, packs3))

## NOTE TO READERS: Adjust this for your local directory:
dataPath <-  "~/Dropbox/WTO-Data/"

## This has the WTO paragraph-level data,
## Meetings 01-113
## post-processing cleanup
## and wealth info
##

speakers.meta <- read_excel(paste0(
    dataPath,"WTOSpeakerTurnsM1to113.xlsx"))

dim(speakers.meta) ##8854 x 15

colnames(speakers.meta) ##
summary(speakers.meta)

head(speakers.meta$date) ## 1995-04-04 UTC
summary(speakers.meta$date)

## Correct some misencoded meeting dates for M110-113
speakers.meta[which(speakers.meta$meetingno==110),
              "date"] <- as.Date("2019-11-22")

speakers.meta[which(speakers.meta$meetingno==111),
              "date"] <- as.Date("2020-05-26")

speakers.meta[which(speakers.meta$meetingno==112),
                  "date"] <- as.Date("2020-09-29")
speakers.meta[which(speakers.meta$meetingno==113),
              "date"] <- as.Date("2020-11-20")

speakers.meta$date <- as.Date(speakers.meta$date,
                              format="%Y-%m-%d")

summary(speakers.meta$date)

## Verify no missing data in the columns we'll use:
sum(is.na(speakers.meta$docid))
sum(is.na(speakers.meta$country))
sum(is.na(speakers.meta$date))
sum(is.na(speakers.meta$cleanedtext)) ## 3
 sum(is.na(speakers.meta$firstent))
 sum(is.na(speakers.meta$pid)) ##1
summary(speakers.meta)

## add a missing PID:
speakers.meta[which(is.na(speakers.meta$pid)), "pid"] <- 21211##

## The one NA is an empty row:
speakers.meta <- speakers.meta %>%
    filter(!is.na(cleanedtext))

dim(speakers.meta) ## 8851

### Add numdate for the STM model

speakers.meta$numdate <- as.numeric(speakers.meta$date)

dim(speakers.meta)

############################
### Duplication analysis
## A downstream analysis suggested that
## somewhere in the data, about 1% of the paragraphs became
## duplicated
################################

## Overall:

minhash <- minhash_generator(200, seed = 6889)

reused <- TextReuseCorpus(text=speakers.meta$cleanedtext,
                          meta=list(
                              pid=speakers.meta$pid,
                              meeting=speakers.meta$meetingno),
                          tokenizer=tokenize_ngrams,
                          minhash_func = minhash)


key <- as.data.frame(cbind(
    docid=names(reused$documents),
    pid=reused$meta$pid))

head(key)

buckets <- lsh(reused, bands = 100, progress = FALSE)

candidates <- lsh_candidates(buckets)

scores <- lsh_compare(candidates, reused,
                      jaccard_similarity,
                      progress = FALSE)

class(scores)

scores <- scores[order(scores$score),]

num.match <- scores[which(scores$score==1),]
num.vsim <- scores[which(scores$score>.75),]
num.sim <- scores[which(scores$score>.5),]

dim(num.match) ## 815 x 3
dim(num.vsim) ##1808

head(num.match)

check <- num.match %>% left_join(key,
                                 by=c("a"= "docid"),
                                 keep=TRUE) %>%
    rename(pid.a = pid,
           docid.a= docid)


check <- check %>%  left_join(key,
                              by=c("b"= "docid"),
                              keep=TRUE)

check <- check %>% rename(pid.b = pid,
                          docid.b = docid)

check$pid.a <- as.numeric(check$pid.a)
check$pid.b <- as.numeric(check$pid.b)

check <-  check %>% left_join(
    speakers.meta[,c("pid", "cleanedtext",
                     "meetingno", "firstent")],
    by=c("pid.a" = "pid")) %>%
    rename(cleanedtext.a = cleanedtext,
           meetingno.a = meetingno,
           firstent.a = firstent) %>%
    left_join(
        speakers.meta[,c("pid", "cleanedtext",
                         "meetingno", "firstent")],
        by=c("pid.b" = "pid")) %>%
    rename(cleanedtext.b = cleanedtext,
           meetingno.b = meetingno,
           firstent.b = firstent)

print(check[,c("meetingno.a", "meetingno.b")],n=815)
print(check[,c("firstent.a", "firstent.b")],n=815)

speakers.a <- as.data.frame(table(check$firstent.a))
speakers.b <- as.data.frame(table(check$firstent.b))

dim(speakers.a) ## 50.2
dim(speakers.b) ##51 x 2

ents <- rbind(speakers.a, speakers.b)

## number attributed to Chair, committe, non-state, GCC:
non.state.speakers <- c("Technical Cooperation and Training Division", "WTO",
                        "UNIDO", "Secretariat",
                        "NS", "GCC",
                        "Development Division",
                        "Deputy Director-General",
                        "Committee", "Chairman",
                        "CARICOM", "Arab Maghreb Union")


## 706 / 815 in the non-state speakers:
dim(check[which(check$firstent.a %in% non.state.speakers |
                check$firstent.b %in% non.state.speakers),])

deleg.check <- check[!(check$firstent.a %in%
                       non.state.speakers |
                       check$firstent.b %in%
                       non.state.speakers),]

nsa.check <- check[(check$firstent.a %in%
                       non.state.speakers |
                       check$firstent.b %in%
                       non.state.speakers),]

dim(deleg.check) ## 109 x 16
dim(nsa.check)## 706 x 16

## 109 pairs remaining; worth checking these by hand.
## to make sure that they are 1-1 duplicate pairs
## wheras the non-state duplicates are often many-to-one
## when phrases get reused a lot

## Tricky part for the state subset:
## Merge in with the downstream hand-classified, texts which had a few duplicates:

class2<- read.csv("wto-hand-class500.csv")

dim(class2) ## 487

did.code<-class2$PID

## make list to remove:
## first remov the pids in class2$PID list;
## then choose one from each of the lists

length(deleg.check$pid.a[check$pid.a %in% did.code]) ## 24
length(deleg.check$pid.b[check$pid.b %in% did.code]) ## 14

## make a column for coded:

delg.check$coded.a <- 0
deleg.check$coded.b <- 0
delg.check[which(deleg.check$pid.a %in% did.code),
      "coded.a"] <- 1
deleg.check[which(deleg.check$pid.b %in% did.code),
      "coded.b"] <- 1

table(deleg.check$coded.a) ##85 0, 24 1
table(deleg.check$coded.b) ##95 0, 14 1

table(deleg.check$coded.a, deleg.check$coded.b) ## 8 coded by both

## Confirm that these are 1-1 pairs
## (the non-delegate pairs are often many-to-one clusters)
length(unique(deleg.check$pid.a)) ##109
length(unique(deleg.check$pid.b)) ##109

## Read this by hand, can't get a good enough view
## in R:
##write.csv(deleg.check,
##          file="delegDuplicatesToCheck.csv")

## decision-rule for the delegate pairs:
## If the "b" one has been coded (coded.b==1), take that one
## else take the "a" text:

deleg.check$select <- ifelse(
    deleg.check$coded.b==1, "b",
    "a")

table(deleg.check$select)
## list of pids to drop in the data for analysis:
## if "b" is selected, pid out is pid.a
## if "a" is selected, pid.out is pid.b
## then take pid.b from all non-delegation
## b/c those have not been coded anyway:

deleg.check$excise <- ifelse(
    deleg.check$select=="b", deleg.check$pid.a,
    deleg.check$pid.b)

omit <- c(deleg.check$excise, nsa.check$pid.b)

## This creates the data that we'll work with,
## without duplicate paragraphs:
speakers.meta2 <- speakers.meta[!(speakers.meta$pid
                                  %in% omit),]

dim(speakers.meta2) ##8456 x 16

#################
##### Clean Text for Topic Models
##################

pageMarkupStopWords <- c("hyperref", "toc", "wtcomtdw",
                         "pageref") ## not meeting content

processed <- textProcessor(documents=speakers.meta2$cleanedtext,
                           metadata=speakers.meta2,
                           removenumbers=TRUE, 
                           customstopwords=pageMarkupStopWords)

summary(processed) ## 8851 documents (paragraphs), 7109  word dictionary ()

out <- prepDocuments(processed$documents,
                     processed$vocab,
                     processed$meta) ## removed 22242 terms due to frequency

## rename objects for ease of reference:
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

## %%%%%%%%%%%%%%%%%%%%%%%%%
## Part 2: K=2 Meta Classification Model
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Development script: 02K2ClassificationModelRepl.R
### Preliminary analysis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data
## This specification: Model covariates by year with faction
## content covariates

packs <- c('tm', 'stm', 'pdftools',
           'tidyr', 'quanteda', "wbstats")

loadPkg(packs)

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

## ugly patch
## To pipe both into differnet names;
## while I decide if the year-only specification is what I want
## to work with:

mod.out.2.cat <- mod.out.2
mod.out.2.fac <- mod.out.2
prep.2.cat <- prep.2
prep.2.fac <- prep.2


##%%%%%%%%%%%%%%%%%%%%%
## Part 3: Model within the "Programs"
## Sub theme

## Development script:  03APerspectivesThemeProgramsYearStemmedFacRepl.R
### Preliminary analysAis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

packs <- c('tm', 'stm', 'pdftools',
           'tidyr', 'quanteda',
           "plm", 'sandwich',
           "stringi")
loadPkg(packs)

mod.out.2.meta <- out$meta

## Extract paragraph-level topic assignments
## round to two
theta2.out.fac <- as.data.frame(round(mod.out.2$theta, 2))  

colnames(theta2.out.fac) <- gsub(pattern="V",
                             replace="M2.Topic",
                             x=as.character(colnames(
                                 theta2.out.fac)))

theta2.out <- cbind(out$meta$pid, theta2.out.fac)

#### Assign dominant topic:
theta2.out$assignedtopic <- colnames(theta2.out[,2:3])[apply(
                                theta2.out[,2:3],1,which.max)] 

table(theta2.out$assignedtopic)

round(prop.table(table(theta2.out$assignedtopic)),2)

head(theta2.out)

paraTopicsK2 <- merge(x=mod.out.2.meta,
                    y=theta2.out,
                    by.x="pid",
                    by.y="out$meta$pid")
dim(paraTopicsK2) ## 5115 x 19

as.data.frame(table(paraTopicsK2$firstent))

## Table A1
##  Summary of delegations:
programs.delegs <- as.data.frame(
    table(paraTopicsK2[which(
        paraTopicsK2$assignedtopic=="M2.Topic2"),
                       ]$firstent))

programs.delegs <- programs.delegs[rev(
    order(programs.delegs$Freq)),]

process.delegs <- as.data.frame(
    table(paraTopicsK2[which(
        paraTopicsK2$assignedtopic=="M2.Topic1"),
                       ]$firstent))

process.delegs <- process.delegs[rev(
    order(process.delegs$Freq)),]

process.delegs[1:5,] ## Table A1.1
programs.delegs[1:5,] ## Table A1.2
 
## Table A2
##  Summary of income level activity:
programs.inc <- as.data.frame(
    table(paraTopicsK2[which(
        paraTopicsK2$assignedtopic=="M2.Topic2"),
                       ]$income_level_iso3c))

programs.inc <- programs.inc[rev(
    order(programs.inc$Freq)),]

process.inc <- as.data.frame(
    table(paraTopicsK2[which(
        paraTopicsK2$assignedtopic=="M2.Topic1"),
                       ]$income_level_iso3c))

process.inc <- process.inc[rev(
    order(process.inc$Freq)),]

process.inc[1:5,] ## Table A2.1
programs.inc[1:5,] ## Table A2.2

## Table A3
##  Summary of faction activity:
programs.faction <- as.data.frame(
    table(paraTopicsK2[which(
        paraTopicsK2$assignedtopic=="M2.Topic2"),]$factio))

programs.faction <- programs.faction[rev(
    order(programs.faction$Freq)),]

process.faction <- as.data.frame(
    table(paraTopicsK2[which(
        paraTopicsK2$assignedtopic=="M2.Topic1"),]$faction))

process.faction <- process.faction[rev(
    order(process.faction$Freq)),]

process.faction[1:3,] ## Table A3.1
programs.faction[1:3,] ## Table A3.2

############################
## Identify the theorized shocks
## In the second theme (Programming)
############################

## Subset to Topic 2:
## Programs-dominant paragraphs:
programsParas <- paraTopicsK2[which(
    paraTopicsK2$assignedtopic=="M2.Topic2"),]

dim(programsParas) ##3041 x 31

colnames(programsParas)

 
## Subset One: Clean the Programs paragraphs

programsParas$cleanedtext <- gsub("[[:digit:]]", "",
                                  programsParas$paratext)

programsParas$cleanedtext <- gsub("[[:punct:]]", "",
                                  programsParas$paratext)

data2 <- corpus(programsParas, text_field = 'paratext')
docvars(data2)$text <- as.character(data2)

length(data2)

dat2 <- dfm(data2,
            stem = TRUE,
            remove = stopwords('english'),
            remove_punct = TRUE) %>%
    dfm_trim(min_termfreq = 2)

out <- convert(dat2, to = 'stm')

dim(out$meta) ## 3041
colnames(out$meta)



#### Model on specific subset:

faction.model= ~s(year)*faction
income.model = ~s(year)*income_level_iso3c

set.seed(6889)
### Income model
mod.programs.themeincome <- stm(documents=out$documents,
                            vocab=out$vocab,
                            data=out$meta,
                            K=10, ## 
                            prevalence=income.model,
                            content= ~income_level_iso3c,
                            seed=61921)

prep.programs.themefaction <- estimateEffect(
    c(1:10)
    ~s(year)* income_level_iso3c,
    mod.programs.themeincome,
    metadata=out$meta,
    documents=out$documents,
    uncertainty=c("Global"))

summary(mod.programs.themefaction)
summary(mod.programs.themeincome)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Part Four: Analysis of Prevalence of
## Topic Two in Reciprocator/Redistributor
## Frames
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Development script:
## 05BmodelAnalysisAcrossAlgorithms500Repl.R
### Preliminary analysis of trade and development
## Compare if frame regression model would give different
## results for different classification algorithems

## modified to take more than the 250
## finishing the branch, though should
## Move towards making a Make file

packs <- c('dotwhisker')

loadPkg(packs)

#############################
## Load Processed Data
#############################

## This loads the K=2 meta analysis
##load("twoTopicsAndSubSets-NoAdminSubset_CatFacRepl.Rdata")

## This loads the K=10 | Topic for K=2 is Programs
##load("programsSubSetstmYearStemmedFacIncRepl.Rdata")

## This loads the predictions on Redist/Reciprocator
load("predicted-models500Repl.Rdata")

## Hand-Tagged:
tags.hand <- read.csv("../wto-hand-class500.csv")

colnames(tags.hand)
tags.hand$X <- NULL
table(tags.hand$.pred_class) ## Recip 153; Redist 292; Unknown 42

## Predicted models are:
## wto.hand; wto.nb.aug; wto.rf.aug
## wto.svm.aug

cols <- c("PID", ".pred_class",
          ".pred_Recip", ".pred_Redist",
          ".pred_Unknown")

rf.preds <- rbind(wto.rf.aug[,cols],
                       tags.hand[,cols])

nb.preds <- rbind(wto.nb.aug[,cols],
                       tags.hand[,cols])

svm.preds <- rbind(wto.svm.aug[,cols],
                       tags.hand[,cols])

glm.preds <- rbind(wto.glm.aug[,cols],
                   tags.hand[,cols])

delg.preds <- wto.hand ## already added together

delg.preds2 <- delg.preds[,c("PID", ".pred_class",
                            ".pred_Recip", ".pred_Redist",
                            ".pred_Unknown")]

## Sanity check-- should all be the same & 8854
dim(rf.preds)
dim(nb.preds)
dim(svm.preds)
dim(glm.preds)
dim(delg.preds2)  ## this has 9 fewer?

##%%%%%%%%%%%%%%%%%%%%%%%%
## Attach topics to paragraphs

## Want the model with K=10| Meta = Programs
## Specification via Income Content Covariate
## Frame
mod2.out <- as.data.frame(
    round(mod.programs.themeincome$theta,2))

## mod2.out <- as.data.frame(round(
##     mod.out.theme2$theta, 2))  ## round to two

colnames(mod2.out) <- gsub(pattern="V",
                           replace="MT2.Topic",
                           x=as.character(
                               colnames(mod2.out)))

mod2.out <- cbind(out$meta$pid, mod2.out)

summary(mod2.out)
 
## README: 2/21: I'm not sure why this works in the standalone
## but in the replication it is perfectly predicted?

mod2.out$assignedtopic <- colnames(mod2.out[,2:11])[apply(
    mod2.out[,2:11],1,which.max)]

table(mod2.out$assignedtopic)

## Summarize topic frequencies:
round(prop.table(table(mod2.out$assignedtopic)), 2)

colnames(paraTopicsK2)
summary(as.factor(paraTopicsK2$assignedtopic)) ## 2074 T1;
## 3041 T2; 0 NA


out3 <- merge(paraTopicsK2, ## Themes 1 & 2 proportions
              mod2.out, ## Theme 2 subset topics
              by.x="pid",
              by.y="out$meta$pid",
              all.x=TRUE)

dim(paraTopicsK2)
dim(mod2.out)

out3[which(out3$firstent=="(Other)"),]
## Attach the predicted and hand-tagged frames
dim(out3) ## 5115 x 35; full set of paragraphs for delegations

## Descriptive Statistics for the Appendix 
table(out3$assignedtopic.x)

as.data.frame(table(as.factor(out3$firstent))) #123 unique
round(prop.table(table(out3$assignedtopic.x)),2) 

## Table A1:
tableA1.1 <- as.data.frame(table(
   out3[which(out3$assignedtopic.x=="M2.Topic1"),]$firstent))
tableA1.1 <- tableA1.1[rev(order(tableA1.1$Freq)),]
tableA1.1[1:5,]

## Table A1:
tableA1.2 <- as.data.frame(table(
    out3[which(out3$assignedtopic.x=="M2.Topic2"),]$firstent))

tableA1.2 <- tableA1.2[rev(order(tableA1.2$Freq)),]

tableA1.2[1:5,]


#### Estimate Regression Models
### Setup:

set.seed(2622)
sendThrough <- function(predictedData, STMData){

    set.seed(2622)
    library('dplyr')
    library('sandwich')
    library('plm')
    
    out4 <- merge(STMData, ## model predictions
                  predictedData,
                  by.x="pid",
                  by.y="PID",
                  all.x=TRUE)
        
    out4$redistributors <- 0
    out4[which(out4$.pred_class=="Redist"),
         "redistributors"] <- 1
    
    out4$reciprocators<- 0
    out4[which(out4$.pred_class=="Recip"),
         "reciprocators"] <- 1

    ## Need topic proportion lag:
    ## M2.Topic1; M2.Topic2
    ## grouped by firstent, then year, then meeting,
    ## then paranum
    tst <- out4 %>%
        arrange(firstent, meetingno, paranum)  %>%
            mutate(lag.M2T2 = lag(M2.Topic2)) 
    
    ## Model:
    ## Topic proportion ~ Shock periods 1:5 + lag
    
    tst.recip <- tst[which(tst$reciprocators==1),]
    tst.redib <- tst[which(tst$redistributors==1),]
    
    tst.neither <- tst[which(tst$redistributors==0 &
                             tst$reciprocators==0),]
    
    ## Regression Form:
    
    form <- as.formula("M2.Topic2 ~ chinashock + FCshock + Xishock + Trumpshock + covidshock + lag.M2T2")
    ## Model with speaker fixed effects:
    recip.2 <- plm(form,
                   data=tst.recip,
                   index=c("firstent"),
                   model="within")
    
    
    redist.2 <- plm(form,
                    data=tst.redib,
                    index=c("firstent"),
                    model="within")
    
    neither.2 <- plm(form,
                     data=tst.neither,
                     index=c("firstent"),
                     model="within")

    outlist <- list(reciprocators=recip.2,
                    redistributors=redist.2,
                    neither=neither.2)
    return(outlist)
}


rf.reg <- sendThrough(predictedData=rf.preds,
                      STMData=out3)

svm.reg <- sendThrough(predictedData=svm.preds,
                       STMData=out3)

nb.reg <- sendThrough(predictedData=nb.preds,
                      STMData=out3)

glm.reg <- sendThrough(predictedData=glm.preds,
                       STMData=out3)

delg.reg <- sendThrough(predictedData=delg.preds2,
                        STMData=out3)

library(dotwhisker)
## Reciprocator Prevalence
recip.plot <- dwplot(list(RF=rf.reg[[1]],
            SVM=svm.reg[[1]],
            GLM=glm.reg[[1]],
            NB=nb.reg[[1]],
            Deleg=delg.reg[[1]]),
       vline = geom_vline(
           xintercept = 0,
           colour = "grey60",
           linetype = 2
           ))  %>% # plot line at zero _behind_coefs
    relabel_predictors(
        c(
            chinashock = "China Joins",
            FCshock = "2008 FC",
            Xishock = "Xi Ascends",
            Trumpshock = "Trump Elected",
            covidshock = "Covid",
            lag.M2T2 = "Deleg. Prev. Turn"
            )) +
    xlab("Coefficient Estimate") +
    ylab("") +
    ggtitle("Frame Prevalence After Shocks",
            subtitle="Reciprocator Paragraphs") +
    theme_bw()


## Redistributor Prevalence

redist.plot <- dwplot(list(RF=rf.reg[[2]],
            SVM=svm.reg[[2]],
            GLM=glm.reg[[2]],
            NB=nb.reg[[2]],
            Deleg=delg.reg[[2]]),
       vline = geom_vline(
           xintercept = 0,
           colour = "grey60",
           linetype = 2
           ))  %>% # plot line at zero _behind_coefs
    relabel_predictors(
        c(
            chinashock = "China Joins",
            FCshock = "2008 FC",
            Xishock = "Xi Ascends",
            Trumpshock = "Trump Elected",
            covidshock = "Covid",
            lag.M2T2 = "Deleg. Prev. Turn"
            )) +
    xlab("Coefficient Estimate") +
    ylab("") +
    ggtitle("Frame Prevalence After Shocks",
            subtitle="Redistributor Paragraphs") +
    theme_bw()


## Neither Reciprocator nor Redistributor

others.plot <- dwplot(list(RF=rf.reg[[3]],
            SVM=svm.reg[[3]],
            GLM=glm.reg[[3]],
            NB=nb.reg[[3]],
            Deleg=delg.reg[[3]]),
       vline = geom_vline(
           xintercept = 0,
           colour = "grey60",
           linetype = 2
           ))  %>% # plot line at zero _behind_coefs
    relabel_predictors(
        c(
            chinashock = "China Joins",
            FCshock = "2008 FC",
            Xishock = "Xi Ascends",
            Trumpshock = "Trump Elected",
            covidshock = "Covid",
            lag.M2T2 = "Deleg. Prev. Turn"
            )) +
    xlab("Coefficient Estimate") +
    ylab("") +
    ggtitle("Frame Prevalence After Shocks",
            subtitle="Other Paragraphs") +
    theme_bw()



###%%%%%%%%%%%%%%%%%%
## Summary Stats
##%%%%%%%%%%%%%%%%%%%
