### Preliminary analysis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

## This specification: Model covariates by year 
## content covariates

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 'pdftools',
           'tidyr', 'quanteda', "wbstats",
           "ggplot2", "gt")

loadPkg(packs)

#############################
## Load Processed Data
#############################

load("twoTopicsAndSubSets-NoAdminSubset_CatFacRepl.Rdata")

attributes(mod.out.2) ## K=2, model= ~(year)

attributes(mod.out.2$settings$call)

## Summary of number of paragraphs in each topic:

theta.summary<- as.data.frame(round(mod.out.2$theta,
                                    2))  ## round to two

colnames(theta.summary) <- gsub(pattern="V",
                                replace="M2.Topic",
                                x=as.character(
                                    colnames(theta.summary)))

theta.summary <- cbind(out$meta$pid, theta.summary)

head(theta.summary)

## ID top topic:
theta.summary$assignedtopic <- colnames(
    theta.summary[,2:3])[apply(
        theta.summary[,2:3],1,which.max)] 

theta.summary[which( ## rename
    theta.summary$assignedtopic=="M2.Topic1"),
              "assignedtopic"] <- "Process"

theta.summary[which(
    theta.summary$assignedtopic=="M2.Topic2"),
              "assignedtopic"] <- "Programs"

## In-text Report: Descrptives: Proportion of topics 
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

## Report: Tech Appendix Figure Overview of
## Dominant Themes

# Plot the model:
twoTopics <- c("Process", "Programs")

## Custom plot legend:
commas <- function(text){
    paste(text[nchar(text)>0],
          collapse=", ")}

label.t1 <- paste0("Process: ",
                   commas( ## paste frex
                          labelTopics(mod.out.2,
                                      n=5)$frex[1,]))

label.t2 <- paste0("Programs: ",
                   commas(
                       labelTopics(mod.out.2,
                                   n=5)$frex[2,]))


png(file="overallThemesK2byyear.png")
plot.estimateEffect(prep.2,
                    model=mod.out.2,
                    covariate="year",
                    main="CTD Corpus Dominant Themes",
                    topics=c(1:2), 
                    method="continuous",
                    xlab="Year",
                    labeltype="custom",
                    printlegend=FALSE,
                    n=5,
                    linecol=c("red", "darkblue")
                    )
legend("topright", legend=c(label.t1, label.t2),
       col=c("red", "darkblue"), lty=1)
dev.off()


## Figure A2: Distribution of topic mixes
## In Factions

gg2 <- ggplot(dat=paraTopicsK2,
              aes(x=M2.Topic1,
                  fill=faction))+
    geom_density(alpha=.3)+
    theme_bw()+
    ggtitle("Distribution of Topic Assignment: Process")

ggsave(gg2, file="DistProcessFaction.png")

gg3 <- ggplot(dat=paraTopicsK2,
              aes(x=M2.Topic2,
                  fill=faction))+
    geom_density(alpha=.3)+
    theme_bw()+
    ggtitle("Distribution of Topic Assignment: Programs")

ggsave(gg3, file="DistProgramsFaction.png")


gg4 <- ggplot(dat=paraTopicsK2,
              aes(x=M2.Topic1,
                  fill=income_level_iso3c))+
    geom_density(alpha=.3)+
    theme_bw()+
    ggtitle("Distribution of Topic Assignment: Process")

ggsave(gg4, file="DistProcessIncome.png")

gg5 <- ggplot(dat=paraTopicsK2,
              aes(x=M2.Topic2,
                  fill=income_level_iso3c))+
    geom_density(alpha=.3)+
    theme_bw()+
    ggtitle("Distribution of Topic Assignment: Programs")


ggsave(gg5, file="DistProgramsIncome.png")

############################
##### Table A1: Summary of delegate activities
## in the two topics
############################

## Process vs Programs Top Delegations
## Table A1.A: Top 5 Process Delegations

## process
t1freq <- as.data.frame(table(paraTopicsK2[which(
    paraTopicsK2$assignedtopic=="Process"),
                                 ]$firstent))
t1freq <- t1freq[rev(order(t1freq$Freq)),]
colnames(t1freq) <- c("Delegation", "No. Turns")

## programs
t2freq <- as.data.frame(table(paraTopicsK2[which(
    paraTopicsK2$assignedtopic=="Programs"),
                                 ]$firstent))
t2freq <- t2freq[rev(order(t2freq$Freq)),]
colnames(t2freq) <- c("Delegation", "No. Turns")

sink(file="ProcessandProgramsTopFive.txt")
write.table(cbind(t1freq[1:5,],t2freq[1:5,]),
            row.names=FALSE,
            sep=",")
sink()

## Table A2: Income level by type
## Process vs Programs By Income Level

## process
t1freq <- as.data.frame(table(
    paraTopicsK2[which(
        paraTopicsK2$assignedtopic=="Process"),
                 ]$income_level_iso3c))
t1freq <- t1freq[rev(order(t1freq$Freq)),]
colnames(t1freq) <- c("Income Group", "No. Turns")

## programs
t2freq <- as.data.frame(table(
    paraTopicsK2[which(
        paraTopicsK2$assignedtopic=="Programs"),
                 ]$income_level_iso3c))
t2freq <- t2freq[rev(order(t2freq$Freq)),]
colnames(t2freq) <- c("Income Group", "No. Turns")


sink(file="ProcessandProgramsSummaryIncLev.txt")
write.table(cbind(t1freq,t2freq),
            row.names=FALSE,
            sep=",")
sink()

## Table A2.B: By Faction

## Table A2: Income level by type
## Process vs Programs By Income Level

## process
t1fac <- as.data.frame(table(
    paraTopicsK2[which(
        paraTopicsK2$assignedtopic=="Process"),
                 ]$faction))
t1fac <- t1fac[rev(order(t1fac$Freq)),]
colnames(t1fac) <- c("Faction", "No. Turns")

## programs
t2fac <- as.data.frame(table(
    paraTopicsK2[which(
        paraTopicsK2$assignedtopic=="Programs"),
                 ]$faction))
t2fac <- t2fac[rev(order(t2fac$Freq)),]
colnames(t2fac) <- c("Faction", "No. Turns")


sink(file="ProcessandProgramsSummaryFaction.txt")
write.table(cbind(t1fac,t2fac),
            row.names=FALSE,
            sep=",")
sink()
