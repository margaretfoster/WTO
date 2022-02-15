## Second pass through, pulling about 500
## paragraphs to hand-tag

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('stm', 'ggplot2',
           'dplyr','tidyr',
           'stminsights')

loadPkg(packs)

ls()

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
## Data 1: K=10 model on Program subset
#############################
load(paste0(dataPathDesktop,
            "ProgramsSubSetstmYearStemmedInc.Rdata"))

## To Do: use the topic assignment code from the K=2 classification to classify the K=10

## Add metadata field with assigned topic:

theta.summary<- as.data.frame(round(mod.prog.themeinc$theta,
                                    2))  ## round to two

colnames(theta.summary) <- gsub(pattern="V",
                                replace="Topic",
                                x=as.character(
                                    colnames(theta.summary)))

theta.summary <- cbind(out$meta$pid, theta.summary)

dim(theta.summary)
head(theta.summary)

## ID top topic:
theta.summary$assignedtopic <- colnames(
    theta.summary[,2:11])[apply(
        theta.summary[,2:11],1,which.max)] 

table(theta.summary$assignedtopic)

paraTopics10 <- merge(x=out$meta,
                    y=theta.summary,
                    by.x="pid",
                    by.y="out$meta$pid")

dim(paraTopics10) ## 3517 x 42

## Goal: make a CSV of FindThoughts for topics of interest
## to tag for frames
 
mod.prog.themeinc$settings$call

texts<- paraTopics10$cleanedtext %>%
    as.character()


thoughts <- findThoughts(
    mod.prog.themeinc,
    n = 25,
    topics = c(1:10),
    texts = texts,
    )

attributes(thoughts)

class(thoughts$index) ## list of length topics
length(thoughts$index) ## 2

topicIndices <- unlist(thoughts$index)

thoughtdf <- paraTopics10 %>%
    slice(topicIndices) %>%
    select(cleanedtext,
           year,
           pid,
           assignedtopic.y)

dim(thoughtdf)

table(thoughtdf$assignedtopic.y) ## 25 of each, great

write.table(thoughtdf,
            sep=";",
            file= "programsTopParas.txt")


## 2/10: second sample
## of about 500

colnames(thoughtdf)
table(thoughtdf$assignedtopic.y)

already.tagged<- thoughtdf$pid
topics.to.select <- c("Topic1", "Topic3", "Topic4",
                      "Topic5", "Topic6", "Topic7",
                      "Topic8", "Topic9", "Topic10")


dim(paraTopics10) ## 3517

dat2<- paraTopics10[!(paraTopics10$pid %in%
                             already.tagged) &
                           paraTopics10$assignedtopic.y %in%
                           topics.to.select,]

dim(dat2) # 2615

set.seed(21022)

select.ids <- sample(x=dat2$pid,
                     size=500,
                     replace=FALSE)

cols <- c("pid", "cleanedtext", "assignedtopic.y")
second.set <- dat2[which(dat2$pid %in% select.ids),
                   cols]

dim(second.set) ## 500 x 3

write.csv(second.set,
          file="secondtaggingSet.csv")

