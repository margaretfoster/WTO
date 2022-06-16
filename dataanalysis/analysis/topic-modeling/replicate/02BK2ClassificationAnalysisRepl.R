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
           "ggplot2")

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


## Report: Tech Appendix Figure Overview of
## Dominant Themes

# Plot the model:
twoTopics <- c("Procedural",
               "Substantive")

## Custom plot legend:
commas <- function(text){
    paste(text[nchar(text)>0],
          collapse=", ")}

tnames <- c("Procedural Matters",
            "Substantive Matters")

label.t1 <- paste0("Procedural Matters: ",
                   commas( ## paste frex
                          labelTopics(mod.out.2,
                                      n=5)$frex[1,]))

label.t2 <- paste0("Substantive Matters: ",
                   commas(
                       labelTopics(mod.out.2,
                                   n=5)$frex[2,]))

## This plot is for the K2 corpus dominant meta
## theme
png(file="overallThemesK2byyear.png")
par(bty="n",lwd=2,xaxt="n") 
plot.estimateEffect(prep.2,
                    model=mod.out.2,
                    covariate="year",
          
                    topics=c(1:2), 
                    method="continuous",
                    xlab="Year",
                    labeltype="custom",
                    printlegend=FALSE,
                    n=5,
                    xaxt="n",
                    linecol=c("darkred",
                        "darkgreen")
                    )
abline(v=c(2002, 2003,
        2009,2010,
        2013,2014,
        2017,2018,
        2020, 2021),
       lty=2, lwd=2,col="grey")
par(xaxt="s")
axis(1,at=c(1995:2021),
     labels=c(1995:2021),
     las=2)
legend("topright", legend=tnames,
       col=c("darkred", "darkgreen"),
       lty=1,
       box.lwd = 0,box.col = "white",
       bg = "white")
dev.off()

## Figure A2: Distribution of topic mixes
## In Factions

gg2 <- ggplot(dat=paraTopicsK2,
              aes(x=M2.Topic1,
                  fill=faction))+
    geom_density(alpha=.3)+
    theme_bw()+
    guides(fill=guide_legend(title="Key"))+
    theme(legend.position="none")+
    scale_fill_grey(start = .4, end = .9)+
    ## ggtitle("Procedural Matters") +
    xlab("STM Assignment To Procedural Matters") +
   ylab("Distribution Density") +
    facet_wrap(~faction, ncol=1)

gg2
ggsave(gg2, file="DistProceduralFaction.png")

gg3 <- ggplot(dat=paraTopicsK2,
              aes(x=M2.Topic2,
                  fill=faction))+
    geom_density(alpha=.3)+
    theme_bw()+
    guides(fill=guide_legend(title="Key"))+
    scale_fill_grey(start = .4, end = .9)+
   ## ggtitle("Substantive Matters")+
    xlab("STM Assignment To Substantive Matters") +
   ylab("Distribution Density") +
    facet_wrap(~faction, ncol=1)+
    ##        theme(axis.text.x = element_text(angle = 45))  


gg3
ggsave(gg3, file="DistSubstanceFaction.png")

## Distribution of topic assignments
## According to Speaker-Turns
## By Faction

dateRanges <- data.frame(
    start = c(2002, 2009, 2013,2017,2020),
    end= c(2003, 2010, 2014, 2018, 2021)
    )

dateRanges$start.m1 <- dateRanges$start - .5
dateRanges$end.p1 <- dateRanges$end + .5

gg.proc <- ggplot(dat=paraTopicsK2,
                 aes(y = M2.Topic1,
                     x=year)) +
    geom_point(alpha=.25) +
    ylab("STM Assignment To Procedural Matters") +
    geom_rect(data = dateRanges,
              aes(xmin = start.m1 ,
                  xmax = end.p1, ymin = -Inf, ymax = Inf),
              inherit.aes=FALSE,
              alpha = 0.5, fill = c("gray"))+
    ##ggtitle("Speaker-Turn Topic Assignment: Procedural Matters")+
    scale_x_continuous(breaks=c(1995:2021)) +
    facet_wrap(~faction, ncol=1)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45))  

gg.proc
ggsave(gg.proc, file="TimeSeriesProceduralFaction.png")


gg.sub <- ggplot(dat=paraTopicsK2,
                 aes(y=M2.Topic2,
                     x=year,
                     group=firstent))+
    geom_point(alpha=.25) +
           geom_rect(data = dateRanges,
                 aes(xmin = start.m1 ,
                     xmax = end.p1, ymin = -Inf, ymax = Inf),
                 inherit.aes=FALSE,
                     alpha = 0.5, fill = c("gray"))+
        scale_x_continuous(breaks=c(1995:2021)) +
    ## ggtitle("Speaker-Turn Assignment: Substantive Matters")+
        ylab("STM Assignment To Substantive Matters") +
    facet_wrap(~faction, ncol=1)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45))  

gg.sub
ggsave(gg.sub, file="TimeSeriesSubstanceFaction.png")

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

## Plot: Proportion of speaker-turns across time in each faction
library(dplyr)


factionSum <- paraTopicsK2 %>% 
    group_by(year, faction) %>% 
    summarize(count=n()) %>%
    group_by(year) %>%
    mutate(proportion=count/sum(count))

dim(factionSum) ## 78x 3
head(factionSum)

gg6 <- ggplot(factionSum,
              aes(y = proportion,
                  x = year)) + 
    geom_area(aes(fill = faction), 
              alpha = 0.5) +
    guides(fill=guide_legend(title="Key"))+
    scale_fill_grey(start = .4, end = .9)+
    ylab("Proportion of Speaker Turns")+
    xlab("Committee Year")+
    theme_bw() +
    scale_x_continuous(breaks=c(1995:2021)) +
    theme(axis.text.x = element_text(angle = 45))  

    ## ggtitle("Factional Activity Across Time in CTD")

gg6

ggsave(gg6,
       file="FactionProportionTime.png")


## Plot: Proportion across time in each income grouping


incomeSum <- paraTopicsK2 %>% 
    group_by(year, income_level_iso3c) %>% 
    summarize(count=n()) %>%
    group_by(year) %>%
    mutate(proportion=count/sum(count))

incomeSum$proportion <- round(incomeSum$proportion, 2)
dim(incomeSum) ## 127 x 4
head(incomeSum)

incomeSum$income_level_iso3c <- factor(
    incomeSum$income_level_iso3c,
    levels=c("LIC", "LMC", "UMC", "HIC"))


gg7 <- ggplot(incomeSum,
              aes(y = proportion,
                  x = year)) + 
    geom_area(aes(fill = income_level_iso3c), 
              alpha = 0.5) +
    guides(fill=guide_legend(title="Income"))+
    ylab("Proportion of Delegate Turns")+
    xlab("Committee Year")+
    theme_bw() +
    ggtitle("Activity Across Time in CTD")

gg7

ggsave(gg7,
       file="IncProportionTime.png")


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
