### Preliminary analysis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

## This specification: Model covariates by year 
## content covariates

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib,
                               repos='http://cran.rstudio.com/')}
        suppressMessages( library(lib, character.only=TRUE))}}


packs <- c('tm', 'stm',
           'pdftools',
           "wbstats",
           "tidytext",
           "ggplot2",
           "ggcorplot")

loadPkg(packs)

#############################
## Load Processed Data
#############################

load("twoTopicsAndSubSets-NoAdminSubset_CatFacRepl.Rdata")

## Review summary stats:
## Summary stats for speaker frequency &
## length of speaker-turns
ls()

dim(out$meta) ##5115
dim(meta)

## attributes(mod.out.2$settings$call)

## Summary of number of paragraphs in each topic:


theta.summary<- as.data.frame(round(mod.out.2$theta,
                                    2))  ## round to two

colnames(theta.summary) <- gsub(pattern="V",
                                replace="M2.Topic",
                                x=as.character(
                                    colnames(theta.summary)))

theta.summary <- cbind(out$meta$pid, theta.summary)


## ID top topic:
theta.summary$assignedtopic <- colnames(
    theta.summary[,2:3])[apply(
        theta.summary[,2:3],1,which.max)] 

theta.summary[which( ## rename
    theta.summary$assignedtopic=="M2.Topic1"),
              "assignedtopic"] <- "Procedural"

theta.summary[which(
    theta.summary$assignedtopic=="M2.Topic2"),
              "assignedtopic"] <- "Substantive"

## In-text Report: Descriptives: Proportion of topics 
table(theta.summary$assignedtopic)
round(prop.table(table(
    theta.summary$assignedtopic)),2)

head(theta.summary)

## Bring in metadata
paraTopicsK2 <- merge(x=out$meta,
                    y=theta.summary,
                    by.x="pid",
                    by.y="out$meta$pid")

dim(paraTopicsK2) ## 5115 x 31

table(paraTopicsK2$faction)
paraTopicsK2$faction <- factor(paraTopicsK2$faction,
                               levels = c("China-Egypt-India",
                                   "US-EU-Can",
                                   "Other"))

##Kolmogorov–Smirnov Test of Different Distributions inside
## & Outside crisis 

## Meta "is shock"
table(paraTopicsK2$chinashock) ## 601
table(paraTopicsK2$FCshock) ## 228
table(paraTopicsK2$Xishock) ## 301
table(paraTopicsK2$Trumpshock) ## 383
table(paraTopicsK2$covidshock) ## 51
      

paraTopicsK2$isshock <- 0
paraTopicsK2[which(
    paraTopicsK2$chinashock== 1 |
    paraTopicsK2$FCshock == 1 |
    paraTopicsK2$Xishock == 1 |
    paraTopicsK2$Trumpshock == 1 |
    paraTopicsK2$covidshock == 1), "isshock"] <- 1

table(paraTopicsK2$isshock) ## 3551 0; 1564 1

## for all [delegate] speaker turns
noshock <- paraTopicsK2[which(paraTopicsK2$isshock==0),
                        "M2.Topic2"]
isshock <- paraTopicsK2[which(paraTopicsK2$isshock==1),
                        "M2.Topic2"]
length(noshock)

ks.test(noshock, isshock) ## Difference is statistically significant


noshock.f1 <- paraTopicsK2[which(
    paraTopicsK2$isshock==0 &
    paraTopicsK2$faction== "China-Egypt-India"),
                        "M2.Topic2"]

isshock.f1 <- paraTopicsK2[which(
    paraTopicsK2$isshock==1 &
    paraTopicsK2$faction== "China-Egypt-India"),
                           "M2.Topic2"]

ks.test(noshock.f1,
        isshock.f1) ## Difference is statistically significant


noshock.f2 <- paraTopicsK2[which(
    paraTopicsK2$isshock==0 &
    paraTopicsK2$faction== "US-EU-Can"),
                        "M2.Topic2"]

isshock.f2 <- paraTopicsK2[which(
    paraTopicsK2$isshock==1 &
    paraTopicsK2$faction== "US-EU-Can"),
                           "M2.Topic2"]

ks.test(noshock.f2,
        isshock.f2) ## Difference is statistically significant


noshock.f3 <- paraTopicsK2[which(
    paraTopicsK2$isshock==0 &
    paraTopicsK2$faction== "Other"),
                        "M2.Topic2"]

isshock.f3 <- paraTopicsK2[which(
    paraTopicsK2$isshock==1 &
    paraTopicsK2$faction== "Other"),
                           "M2.Topic2"]

ks.test(noshock.f3,
        isshock.f3) ## Difference is statistically significant

## Correlation Plot

colnames(paraTopicsK2)

              
