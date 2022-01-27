## Measure 

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tidyr', 'quanteda',
           "classInt",
           "countrycode")
packs.quanteda <- c("quanteda.textmodels",
                    "quanteda.textplots",
                    "textreuse", "text2vec")

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
##### Summary
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

#################################
## Duplification analysis
################################

## Overall:
## (including administrative and metadata paragraphs)
ls() ## tst is a corpus of 2,285 documents


minhash <- minhash_generator(200, seed = 6889)
reused <- TextReuseCorpus(text=out$meta$cleanedtext,
                          meta=out$meta,
                          tokenizer=tokenize_ngrams,
                         minhash_func = minhash)

buckets <- lsh(reused, bands = 100, progress = FALSE)
candidates <- lsh_candidates(buckets)

scores <- lsh_compare(candidates, reused,
                      jaccard_similarity, progress = FALSE)

class(scores)

scores <- scores[order(scores$score),]

num.match <- scores[which(scores$score==1),]
num.sim <- scores[which(scores$score>.5),]


dim(num.match)

dim(num.sim)/dim(out$meta)

plot(scores$score,
     cex=.15,
     main="Similarity of Speaker-Turns",
     ylab="Jaccard Similarity of Ngrams")


head(scores)

attributes(reused)
length(reused)

dim(out$meta)

## Just country-level speakers

minhash <- minhash_generator(200, seed = 6889)
reused.subset <- TextReuseCorpus(text=dat2$cleanedtext,
                          meta=dat2,
                          tokenizer=tokenize_ngrams,
                         minhash_func = minhash)

buckets.ss <- lsh(reused.subset, bands = 100, progress = FALSE)
candidates.ss <- lsh_candidates(buckets.ss)

scores.ss <- lsh_compare(candidates.ss,
                      reused.subset,
                      jaccard_similarity, progress = FALSE)

scores.ss <- scores.ss[order(scores.ss$score),]

num.match.subset <- scores.ss[which(scores.ss$score==1),]
num.sim.subset <- scores.ss[which(scores.ss$score>.5),]

head(num.match.subset)

dim(num.match.subset)
dim(num.sim.subset)

dim(num.match.subset)/dim(dat2)
dim(num.sim.subset)/dim(dat2)

plot(scores$score,
     cex=.15,
     main="Similarity of Speaker-Turns",
     ylab="Jaccard Similarity of Ngrams")


head(scores)

attributes(reused)
length(reused)

dim(out$meta)
