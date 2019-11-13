##
## Script for visualizations of
## the WTO topic modeling

## uses data 
rm(list=ls())

library(stm)

## Load processed data and model:

load("nopriorfit_adminmod.Rdata") ## with admin as the covariate

load("nopriorfit_mod.Rdata") ## model with trump binary

load("WTOParagraphDataForSTM.Rdata") ## underlying data
load("WTObasemodel-out.Rdata")
## visualization and results


#### Estimate effect:                                                                                                                                        

npf.mod.ee <- estimateEffect(1:50~ trump*refbig5,
                             nopriorfit.mod ,
                             meta=out$meta,
                             uncertainty="Global")


npf.mod.ee2 <- estimateEffect(1:50~ admin*refbig5,
                              nopriorfit.mod2 ,
                              meta=out$meta,
                              uncertainty="Global")


### getting names:                                                                                                                                           

nopriorfit.mod.labels <- labelTopics(nopriorfit.mod)
adminmod.labels <- labelTopics(nopriorfit.mod2)

adminmod.labels
nopriorfit.mod.labels

## Summaries: 
nopriorfit.mod.labels$covariate                                           
adminmod.labels$covariate

## Changes in topic prevalence


## topic 26 and 28 very more common after trump                                                                                                            
## also ...48?                                                                                                                                               
## after                                                                                                                                                     
## come back and clean up this plot, but                                                                                                                     
## focus more on what those topics are                                                                                                                       
## and how the difference is                                                                                                                                 



plot.estimateEffect(npf.mod.ee,
                    covariate = "refbig5",
                    topics=c(26, 28, 48),
                    model = nopriorfit.mod,
                    method = "difference",
                    cov.value1="United States",
                    cov.value2="other",
                    labeltype="frex",
                    verbose.labels = T,
                    moderator = "trump",
                    moderator.value = "0",
                    linecol = "red")



## function for plotting topics by country referred to:                                                                                                      

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
              width=40)
    text(.5,6.5,"Canada", cex=1.1)
    text(.5,5.0,"China", cex=1.1)
    text(.5,4,"Egypt", cex=1.1)
    text(.5,2.5,"India", cex=1.1)
    text(.5,.75,"United States", cex=1.1)

    dev.off()
}



varyByRefs(29)

#### Topics:                                                                                                                                                 

## Label topics is the object that will have the                                                                                                             
## country x post-trump characteristic words:                                                                                                                

labeltopics <- labelTopics(nopriorfit.mod)

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
