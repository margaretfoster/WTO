## Generate figures that show topic proportions by
## faction and framing categories

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

table(thoughtdf$assignedtopic.y) ## 10 of each, great

write.table(thoughtdf,
            sep=";",
            file= "programsTopParas.txt")
