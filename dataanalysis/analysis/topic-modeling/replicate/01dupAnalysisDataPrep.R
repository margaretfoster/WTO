
## Process into the STM format.
## separate central script to customize
## stopwords, stemming
## and have that percolate through rest of analyis

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}

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


key <- as.data.frame(cbind(docid=names(reused$documents),pid=reused$meta$pid))

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

length(omit)

##%%%%%%%%%%%%%%%%%%%%
## Update without the duplicates
## %%%%%%%%%%%%%%%%%%%%

speakers.meta2 <- speakers.meta[!(speakers.meta$pid
                                  %in% omit),]

dim(speakers.meta2) ##8456 x 16

#################
##### Cleanup for STM
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
                     processed$meta) ## removing 2187 due to frequency


## rename objects for ease of reference:
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

ls()

save(out, docs, vocab, meta,
     file=paste0(dataPath, "processedTextforSTMDeDeup.RData"))
 
