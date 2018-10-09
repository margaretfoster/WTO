### check out the results
rm(list=ls())

library(stm)

## if(!require(devtools)) install.packages("devtools")
## library(devtools)
## install_github("mroberts/stmBrowser",dependencies=TRUE)


dataPath <- "../analysis/"

###################################
### load data
###################################

## Estimate effect objects:

load(paste0(dataPath,"estimateEffectTDK20cens.Rdata"))
load(paste0(dataPath, "estimateEffectTDK20.Rdata"))

## STM output objects
load(paste0(dataPath, "tradDevParaK20.RData"))
load(paste0(dataPath, "tradDevParaK20cens.RData"))

ls()
## objects: 
## models:  mod.out.20; mod.out.20.cens
## estimate effect: prep20;  prep20cens

plot(mod.out.20)
plot(mod.out.20.cens)


## Topic correlations:

## relativley high correlation required
cormat20 <- topicCorr(mod.out.20, cutoff=.1)
cormat20cens <- topicCorr(mod.out.20.cens, cutoff=.1)


pdf(file="topicCorsk20cens.pdf")
plot(cormat20cens,
     vertex.size=5,
     vertex.color="lightblue")
dev.off()

pdf(file="topicCorsk20.pdf")
plot(cormat20,
     vertex.size=5,
     vertex.label.cex=1.25,
     vertex.color="lightgreen")
dev.off()




###### Summarize models

par(mfrow=c(1, 1))

pdf(file="topicCorsk20t14.pdf")
plot(mod.out.20,
     topics=1:4,
     type=c("labels"),
     labeltype=c("frex"),
     xlab="STM Output K=20, All Paragraphs") 
dev.off()

pdf(file="topicCorsk20t59.pdf")
plot(mod.out.20,
     topics=5:9,
     type=c("labels"),
     labeltype=c("frex"))
dev.off()

pdf(file="topicCorsk20t1014.pdf")
plot(mod.out.20,
     topics=10:14,
     type=c("labels"),
     labeltype=c("frex"))
dev.off()

pdf(file="topicCorsk20t1520.pdf")
plot(mod.out.20,
     topics=15:20,
     type=c("labels"),
     labeltype=c("frex"))
dev.off()



## cens

pdf(file="topicCorsk20censt14.pdf")
plot(mod.out.20.cens,
     topics=1:4,
     type=c("labels"),
     labeltype=c("frex"),
     xlab="STM Output K=20, All Paragraphs") 
dev.off()

pdf(file="topicCorsk20t59cens.pdf")
plot(mod.out.20.cens,
     topics=5:9,
     type=c("labels"),
     labeltype=c("frex"))
dev.off()

pdf(file="topicCorsk20t1014cens.pdf")
plot(mod.out.20.cens,
     topics=10:14,
     type=c("labels"),
     labeltype=c("frex"))
dev.off()

pdf(file="topicCorsk20t1520cens.pdf")
plot(mod.out.20.cens,
     topics=15:20,
     type=c("labels"),
     labeltype=c("frex"))
dev.off()
