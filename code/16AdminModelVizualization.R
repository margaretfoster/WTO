##
## Script for visualizations of
## the WTO topic modeling

## uses data 
rm(list=ls())

library(stm)

## Load processed data and model:

load("nopriorfit_mod.Rdata") ## model with trump binary, countries as content covariate

load("WTOParagraphDataForSTM.Rdata") ## underlying data
load("WTObasemodel-out.Rdata")

ls()

## To present the frequency of the "big 5"
## at some point want to unlist the columns with more
## than one

references.table <- as.data.frame(table(out$meta$refs))
references.table <- references.table[order(-references.table$Freq),]
head(references.table)


## visualization and results

## plot topics greater than 5% of corpus:
## Cut into intervals of 10 topics:

## confirm that this verson doesn't have "frustrate"
summary(nopriorfit.mod)
plot(nopriorfit.mod)


## grouping topics in descending order of prevalence:
## with 46 as most prevalent, and 3 as least prevalent
top10 <- c(46, 45, 28, 39, 31, 26, 34, 48, 13, 29)
t11to20 <- c(7, 6, 14, 23, 30, 47, 24, 44, 35, 17)
t21to30 <- c(1, 4, 12, 42, 22, 20, 16, 41, 5, 32)
t31to40 <- c(36, 38, 11, 37, 2, 25, 19, 33, 43, 21)
t41to50 <- c(8, 18, 50, 10, 27, 40, 15, 49, 9, 3)


## 10 most common:
png(file="tenmostcommon.png")
par(bty="n",col="navyblue",lwd=5)
plot.STM(nopriorfit.mod, 
         topics=top10,
         main="Top 10 Most Prevalent Topics")
dev.off()

## 11-20  most prevalent
png(file="prevalence11to20mostcommon.png")
par(bty="n",col="darkgreen",lwd=5)
plot.STM(nopriorfit.mod, 
         topics=t11to20,
         main="11-20th Most Prevalent Topics")
dev.off()

png(file="prevalence21to30mostcommon.png")
par(bty="n",col="firebrick",lwd=5)
plot.STM(nopriorfit.mod, 
         topics=t21to30,
         main="21-30th Most Prevalent Topics")
dev.off()


png(file="prevalence31to40mostcommon.png")
par(bty="n",col="darkslategray",lwd=5)
plot.STM(nopriorfit.mod, 
         topics=t31to40,
         main="31-40th Most Prevalent Topics")
dev.off()

png(file="prevalence41to50mostcommon.png")
par(bty="n",col="darkgoldenrod",lwd=5)
plot.STM(nopriorfit.mod, 
         topics=t41to50,
         main="41-50th Most Prevalent Topics")
dev.off()

#####################
### Print content of the topics:
######################
sink(file="detailedK50topicinfo.txt")
sageLabels(nopriorfit.mod, n=7)
sink()

#### Estimate effect:

npf.mod.ee <- estimateEffect(1:50~ trump*refbig5,
                             nopriorfit.mod ,
                             meta=out$meta,
                             uncertainty="Global")


## Name topics
nopriorfit.mod.labels <- labelTopics(nopriorfit.mod)

## label topics, broken into short chunks:
labelTopics(nopriorfit.mod, ## country-covariates
            topics=c(26))


## Summaries: 
nopriorfit.mod.labels$covariate                                           

########
## Changes in topic prevalence
#########

## Overall effect of Trump Admin ==1

## custom labels:
shortlabels <- paste0("T", 1:50)

pdf(file="postTrumpTopicPrevalence.pdf")
par(bty="n",col="darkgoldenrod",lwd=.3, cex=.70)
plot.estimateEffect(npf.mod.ee,
                    covariate= "trump",
                    model=nopriorfit.mod,
                    method="difference",
                    cov.value1="1",
                    cov.value2="0",
                    label="custom",
                    custom.labels=shortlabels,
                    xlim=c(-0.08, 0.08),
                    xlab="Less Prevalent After Trump Admin                                         More Prevalent")
dev.off()

### plot by just US, after Trump Admin
## most common: 28, 48, 22

pdf(file="prepostTrump-USParas.pdf")
par(bty="n",col="chocolate",lwd=.3, cex=.7)
plot.estimateEffect(npf.mod.ee,
                    covariate = "trump",
                    model = nopriorfit.mod,
                    method = "difference",
                    cov.value1="1",
                    cov.value2="0", ## default
                    ##verbose.labels = T,
                    moderator = "refbig5",
                    moderator.value = "United States",
                    labeltype="custom",
                    custom.labels=shortlabels,
                    main="Prevalence of Topics in US-Referencing Paragraphs After Trump Administration",
                    xlab="Less Prevalent                                  More Prevalent")
dev.off()


## function for plotting topics by country referred to:

sagelabs <- sageLabels(nopriorfit.mod)

varyByRefs <- function(topic){

    png(file=paste0("topicVaryingReferencesK_",topic, ".png"))
    plotQuote(c(paste(sagelabs$cov.betas[[1]]$frexlabels[topic,],
                      collapse="\n"),
                paste(sagelabs$cov.betas[[2]]$frexlabels[topic,],
                      collapse="\n"),
                paste(sagelabs$cov.betas[[3]]$frexlabels[topic,],
                      collapse="\n"),
                paste(sagelabs$cov.betas[[4]]$frexlabels[topic,],
                      collapse="\n"),
                paste(sagelabs$cov.betas[[6]]$frexlabels[topic,],
                      collapse="\n")),
              width=30,
              main = paste0("Country-Specific Words for Topic ", topic))
    text(.5,6.5,"Canada", cex=1.1)
    text(.5,5.5,"China", cex=1.1)
    text(.5,3.50,"Egypt", cex=1.1)
    text(.5,2.50,"India", cex=1.1)
    text(.5,1.,"United States",
         cex=1.1, col="darkblue")

    dev.off()
}


## Print the country-level terms for the
## topics that vary by US
varyByRefs(22)
varyByRefs(28)
varyByRefs(48)


## Where the US moves very differently than the rest
## pre/post Trump
### 13, 26, 31, 39, 44

varyByRefs(13)
varyByRefs(26)
varyByRefs(31) ## heights for country names: 2.5...0.5
varyByRefs(39)

varyByRefs(44) ## heights for country names: 6.5, 5.5...1
#### Topics:                                                                                                                                                 

## Label topics is the object that will have the                                                                                                             
## country x post-trump characteristic words:                                                                                                                

labeltopics <- labelTopics(nopriorfit.mod,
                           topics=top10)

## covariate variation by the variale (0/1 on Trum)                                                                                                          

labeltopics$covariate

## What: covariate words by                                                                                                                                  
## trump binary and different levels                                                                                                                         


png("stmMod1to10.png")
plot.STM(nopriorfit.mod,
                  topics=1:10)
dev.off()

png("stmMod11to20.png")
plot.STM(nopriorfit.mod,
                  topics=11:20)
dev.off()

png("stmMod21to30.png")
plot.STM(nopriorfit.mod,
                  topics=21:30)
dev.off()


png("stmMod31to40.png")
plot.STM(nopriorfit.mod,
                  topics=31:40)
dev.off()


png("stmMod41to50.png")
plot.STM(nopriorfit.mod,
                  topics=41:50)
dev.off()



plot.estimateEffect(npf.mod.ee,
                    covariate = "trump",
                    topics = c(3, 7, 20),
                    model = nopriorfit.mod,
                    method = "difference",
                    cov.value1 = "United States",
                    cov.value2 = "China",
                    xlab = "Before Trump Admin ...After Trump Admin",
                     main = "Effect of Trump Administration")


## Descriptive statistics for writeup:                                                                                                                       


colnames(out$meta)
length(unique(out$meta$meeting))
summary(out$meta$date)

pdf("WTO_ParagraphHist.pdf")
hist(out$meta$date,
     breaks="years",
     freq=TRUE,
     main="Distribution of Trade And Development Minutes",
     xlab="Date")
dev.off()


plot(prep, "refbig5",
     topics=c(10),
     method="pointestimate",
     labeltype="custom",
     custom.labels=bigFiveName,
     xlab="Mean topic proportion in corpus")

## cov.betas in sageLabels                                                                                                                                   
dev.off()


par(mfrow=c(1, 2))

plot(prep, "trump",
     method="difference",
     moderator="refbig5",
     moderator.value="United States",
     cov.value1=0,
     cov.value2=1,
     model=model1,
     verbose.labels = FALSE,
     xlab="Change in Topics In Paras Referencing US")



##

##Using Brandon Stewart's code                                          
## From Text for Roberts Et Al,                                           
## "A Model of Text for Experimentation in the Social Sciences"    

sagelabs <- sageLabels(nopriorfit.mod)

##Plotting the words relating to topic 10 for each of the 6 factors.
## proof of concept, not that interesting

plotQuote(c(paste(sagelabs$cov.betas[[1]]$problabels[10,], collapse="\n"),
            paste(sagelabs$cov.betas[[2]]$problabels[10,], collapse="\n"),
            paste(sagelabs$cov.betas[[3]]$problabels[10,], collapse="\n"),
            paste(sagelabs$cov.betas[[4]]$problabels[10,], collapse="\n"),
            paste(sagelabs$cov.betas[[6]]$problabels[10,], collapse="\n")), width=40)
text(.5,4.5,"Ref to Canada", cex=1.1)
text(.5,3.5,"Ref to China", cex=1.1)
text(.5,2.5,"Ref to Egypt", cex=1.1)
text(.5,1.5,"Ref to India", cex=1.1)
text(.5,0.5,"Ref to US", cex=1.1)
