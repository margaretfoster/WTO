## Update 3/7/22: Another pull of paragraphs
## to hand-tag for frame
## Pulling at this point in the chain
## rather than upstream so that I can weight a
## sample between the Process and Programs
rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            {install.packages(lib,
                              repos='http://cran.rstudio.com/') }
        suppressMessages(
            library(lib,
                    character.only=TRUE) ) }}

packs <- c('tm', 'stm', 'pdftools',
           'tidyr', 'quanteda',
           "plm", 'sandwich',
           'stargazer',
           'dotwhisker')

loadPkg(packs)

#############################
## Load Processed Data
#############################

## This loads the K=2 meta analysis
##load("twoTopicsAndSubSets-NoAdminSubset_CatFacRepl.Rdata")

## This loads the K=10 | Topic for K=2 is Programs
load("programsSubSetstmYearStemmedFacIncRepl.Rdata")

## This loads the predictions on Redist/Reciprocator
##load("predicted-models500ReplDelegSubset.Rdata")

## Hand-Tagged:
tags.hand <- read.csv("../wto-hand-class500.csv")

colnames(tags.hand) <- tolower(colnames(tags.hand))
tags.hand$X <- NULL
tags.hand$.pred_class <- tolower(tags.hand$.pred_class)

table(tags.hand$.pred_class) ## Recip 153; Redist 292; Unknown 42

## Standarize the column names in lowercase:

colnames(out$meta) <- tolower(colnames(out$meta))


##%%%%%%%%%%%%%%%%%%%%%%%%
## Attach topics to paragraphs
##%%%%%%%%%%%%%%%%%%%%%%%%%
## Want the model with K=10| Meta = Programs

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

## README: 
## 10 topics:
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


dim(out3) ##5115

## Process PIDs

processPIDs <- out3[which(out3$assignedtopic.x=="M2.Topic1"),]$pid

## Programs PIDs
programsPIDs <- out3[which(out3$assignedtopic.x=="M2.Topic2"),]$pid

length(programsPIDs) ## 3041
length(processPIDs) ## 2074
dim(tags.hand) ## 487x7

## Setdiff for non-tagged PIDS:
process.toSample <- setdiff(processPIDs, tags.hand$pid)
programs.toSample <- setdiff(programsPIDs, tags.hand$pid)

length(process.toSample) ## 2025
length(programs.toSample) #2614

## Sample:  3/4 from programs, 1/4 from Process

1500-dim(tags.hand)[1] ## 1013 brings up to 1500
## 1/2 is about 304; 667

s.process <- sample(size=400,
                    x=process.toSample,
                    replace=FALSE)

s.programs <- sample(size=700,
                    x=programs.toSample,
                    replace=FALSE)

tags.p3 <- out3[which(out3$pid %in%
                      c(s.process, s.programs)),
                c("pid", "firstent", "text")]
                
dim(tags.p3)[1] +hand.ta ##1100

## convert column names to a format needed for Atlas.TI metadata:

colnames(tags.p3) <- c("!pid", "firstent", "text")

write.csv(tags.p3,
          file="handTagSetThree.csv")


