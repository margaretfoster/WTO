
## Process into the STM format.
## separate central script to customize
## stopwords, stemming
## and have that percolate through rest of analyis

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            {install.packages(lib,
                              repos='http://cran.rstudio.com/')}
        suppressMessages(library(lib, character.only=TRUE))}}

packs <- c('tm', 'stm', 'readxl',
           'tidyr', 'quanteda')

packs2 <- c("stringr", "reshape2",
            "dplyr")

packs3 <- c("textclean", "textreuse")

loadPkg(c(packs, packs2, packs3))

## NOTE TO READERS: Adjust this for your local directory:
dataPath <-  "~/Dropbox/WTO-Data/rdatas/"

load("PerspectivesThemeProgramsYearStemmedFacRepl.Rdata")

## Do another round of duplicate analysis
## but this time we're looking for text reuse
## as an indicator of gridlock

## Overall:

minhash <- minhash_generator(200, seed = 22422)

reused <- TextReuseCorpus(text=out$meta$cleanedtext,
                          meta=list(
                              pid=out$meta$pid,
                              meeting=out$meta$meetingno,
                              year=out$meta$year),
                          tokenizer=tokenize_ngrams,
                          minhash_func = minhash)

key <- as.data.frame(cbind(docid=names(reused$documents),
                           pid=reused$meta$pid,
                           meeting=reused$meta$meeting,
                           year=reused$meta$year))

head(key)

buckets <- lsh(reused, bands = 100, progress = FALSE)

candidates <- lsh_candidates(buckets)

scores <- lsh_compare(candidates, reused,
                      jaccard_similarity,
                      progress = FALSE)

class(scores)

scores <- scores[order(scores$score),]

## want to merge in metadata:
scores.meta <- scores %>%
    left_join(key,
              by=c("a"= "docid"),
              keep=TRUE) %>%
    rename(pid.a = pid,
           docid.a= docid,
           meeting.a=meeting,
           year.a=year)

scores.meta <- scores.meta %>%
    left_join(key,
              by=c("b"= "docid"),
              keep=TRUE)%>%
    rename(pid.b = pid,
           docid.b = docid,
           meeting.b=meeting,
           year.b=year)

scores.meta$score <- round(scores.meta$score, 3)

scores.meta$year.a <- format(as.Date(scores.meta$year.a,
                              format="%Y"), "%Y")

scores.meta$year.b <- format(as.Date(scores.meta$year.b,
                              format="%Y"),"%Y")

tail(scores.meta[,c("score", "year.a", "year.b")])


scores.meta <- scores.meta %>%
    rowwise() %>%
    mutate(minmeeting =
           min(c(meeting.a,meeting.b)))%>%
    mutate(maxmeeting =
           max(c(meeting.a,meeting.b)))%>%
    mutate(minyear =
           min(c(year.a,year.b))) %>%
    mutate(maxyear =
           max(c(year.a,year.b)))


tail(scores.meta[,c("minmeeting","maxmeeting")])
## can I find the 81-82-84 meeting?

scores.meta[scores.meta$meeting.a %in c(81, 82, 84)|
            scores.meta$meeting.b %in% c(81, 82, 84),]


library(ggplot2)

gg1 <- ggplot(data=scores.meta,
              aes(x=as.numeric(minyear),
                  y=score))+
    geom_point(size = .75,
               alpha=.55)+
    geom_jitter()+
    scale_x_continuous(breaks = seq(1995, 2020, by=1))+
    geom_text(data=subset(scores.meta,score > .45),
              aes(x= as.numeric(year.a),
                  y=score,
                  label=paste0("M", minmeeting,
                      "- M", maxmeeting)),
              position= position_dodge(0.9),
              check_overlap = TRUE)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45))+
    xlab("Year of First Text") +
    ggtitle("Text Duplication Over Time")+
    ylab("Minhash Similarity Scores")
              

gg1

ggsave(gg1,
       file="HashComparison.png")


#######
