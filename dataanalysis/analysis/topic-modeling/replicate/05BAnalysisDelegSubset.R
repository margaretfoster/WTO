
### Preliminary analysis of trade and development
## Compare if frame regression model would give different
## results for different classification algorithems

## modified to take more than the 250
## finishing the branch, though should
## Move towards making a Make file

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
load("predicted-models500ReplDelegSubset.Rdata")

## Hand-Tagged:
tags.hand <- read.csv("../wto-hand-class500.csv")

colnames(tags.hand) <- tolower(colnames(tags.hand))
tags.hand$X <- NULL
tags.hand$.pred_class <- tolower(tags.hand$.pred_class)

table(tags.hand$.pred_class) ## Recip 153; Redist 292; Unknown 42

## Standarize the column names in lowercase:

colnames(out$meta) <- tolower(colnames(out$meta))

## merge in the metadata to derive statistics about the
## top delegations:

tmp <- merge(meta[,c("pid", "firstent",
                         "income_level_iso3c",
                         "region")],
             tags.hand,
             by.x="pid",
             by.y="pid")

dim(tmp) ## 478 (aka: 51 that got re-assigned to Programs after the re-estimation with no duplicates)

## Some descriptive stats:

as.data.frame(table(tmp[tmp$.pred_class=="Recip", "firstent"]))

as.data.frame(table(tmp[tmp$.pred_class=="Redist", "firstent"]))

rm(tmp)

## Predicted models are:
## wto.hand; wto.nb.aug; wto.rf.aug
## wto.svm.aug


cols <- c("pid", ".pred_class",
          ".pred_recip", ".pred_redist",
          ".pred_unknown")

rf.preds <- rbind(wto.rf.aug[,cols],
                       tags.hand[,cols])

nb.preds <- rbind(wto.nb.aug[,cols],
                       tags.hand[,cols])

svm.preds <- rbind(wto.svm.aug[,cols],
                       tags.hand[,cols])

glm.preds <- rbind(wto.glm.aug[,cols],
                   tags.hand[,cols])

delg.preds <- wto.hand ## already added together

delg.preds2 <- delg.preds[,c("pid", ".pred_class",
                             ".pred_recip",
                             ".pred_redist",
                            ".pred_unknown")]

## Sanity check-- should all be the same & 8854
dim(rf.preds)
dim(nb.preds)
dim(svm.preds)
dim(glm.preds)
dim(delg.preds2)  ## this has 9 fewer; but the number is right for the model

##%%%%%%%%%%%%%%%%%%%%%%%%
## Pull some sample tags:
##%%%%%%%%%%%%%%%%%%%%%

names(rf.preds)
summary(rf.preds)

## Compare the predicted paragraphs in the not-hand-tagged set

rf.pred.recip <- wto.rf.aug[which(
    wto.rf.aug$.pred_class=="recip"),]$pid
rf.pred.redist <- wto.rf.aug[which(
    wto.rf.aug$.pred_class=="redist"),]$pid
rf.pred.other <- wto.rf.aug[which(
    wto.rf.aug$.pred_class=="unknown"), ]$pid

delg.pred.recip <- delg.preds[which(
    delg.preds$.pred_class=="recip"), ]$pid
delg.pred.redist <- delg.preds[which(
    delg.preds$.pred_class=="redist"),]$pid
delg.pred.other <- delg.preds[which(
    delg.preds$.pred_class=="unknown"),]$pid

length(intersect(delg.pred.recip, rf.pred.recip)) ##59 + 153 shared
length(intersect(delg.pred.redist, rf.pred.redist)) ##547 + the shared in hand.tagged


overlap.recip <- intersect(delg.pred.recip, rf.pred.recip)
overlap.recip

sumcols <- c("cleanedtext", "meetingno", "year")

out$meta[which(out$meta$pid==1266),sumcols] ## Japan, good candidate to highlight
out$meta[which(out$meta$pid==1180),sumcols] ## CH looking for "synergies"
out$meta[which(out$meta$pid==7495),sumcols] ## Long EU GSP presentation
out$meta[which(out$meta$pid==8374),sumcols] ## Canada supports APEC
out$meta[which(out$meta$pid==2960),sumcols] ## CH on tech communication, good candidate

sink(file="highlightRecipFrame.txt")
out$meta[which(out$meta$pid==7705), sumcols] ## US on their GSP, example of redistributor preference
sink()


## Example in both redistributor sets: 
overlap.redist <- intersect(delg.pred.redist, rf.pred.redist)
length(overlap.redist) #547

overlap.redist

out$meta[which(out$meta$pid==4415),sumcols] ## Good Uganda example

sink(file="highlightRedistFrame.txt")
out$meta[which(out$meta$pid==4405),sumcols] ## Uganda in M99
sink()


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
## I'm revising this to reflect the fact that there are
## two topics here, but should revisit (and make sure that the
## model is, indeed, supposed to be showing just percent of Programs topics:
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

## Attach the predicted and hand-tagged frames
dim(out3) ## 5115 x 35; full set of paragraphs for delegations

## Descriptive Statistics for the Appendix 
table(out3$assignedtopic.x)
summary(as.factor(out3$assignedtopic.x)) ## 0NA
as.data.frame(table(as.factor(out3$firstent)))

round(prop.table(table(out3$assignedtopic.x)),2) 


## Table A1:
tableA1.1 <- as.data.frame(table(
   out3[which(out3$assignedtopic.x=="M2.Topic1"),]$firstent))
tableA1.1 <- tableA1.1[rev(order(tableA1.1$Freq)),]
tableA1.1[1:5,]

## Table A1:
tableA1.2 <- as.data.frame(table(
    out3[which(out3$assignedtopic.x=="M2.Topic2"),]$firstent))
tableA1.2 <- tableA1.2[rev(order(tableA1.2$Freq)),]

tableA1.2[1:5,]

#### Estimate Regression Models
### Setup:

set.seed(2622)
sendThrough <- function(predictedData, STMData){

    set.seed(2622)
    library('dplyr')
    library('sandwich')
    library('plm')
    
    out4 <- merge(STMData, ## model predictions
                  predictedData,
                  by.x="pid",
                  by.y="pid",
                  all.x=TRUE)
    
    predictedData$.pred_class <- tolower(predictedData$.pred_class)
    
    out4$redistributors <- 0
    out4[which(out4$.pred_class=="redist"),
         "redistributors"] <- 1
    
    out4$reciprocators<- 0
    out4[which(out4$.pred_class=="recip"),
         "reciprocators"] <- 1

    ## Need topic proportion lag:
    ## M2.Topic1; M2.Topic2
    ## grouped by firstent, then year, then meeting,
    ## then paranum
    tst <- out4 %>%
        arrange(firstent, meetingno, paranum)  %>%
            mutate(lag.M2T2 = lag(M2.Topic2)) 
    
    ## Model:
    ## Topic proportion ~ Shock periods 1:5 + lag
    
    tst.recip <- tst[which(tst$reciprocators==1),]
    tst.redib <- tst[which(tst$redistributors==1),]
    
    tst.neither <- tst[which(tst$redistributors==0 &
                             tst$reciprocators==0),]
    
    ## Regression Form:
    
    form <- as.formula("M2.Topic2 ~ chinashock + FCshock +
Xishock + Trumpshock + covidshock + lag.M2T2")
    ## Model with speaker fixed effects:
    recip.2 <- plm(form,
                   data=tst.recip,
                   index=c("firstent"),
                   model="within")
    
    
    redist.2 <- plm(form,
                    data=tst.redib,
                    index=c("firstent"),
                    model="within")
    
    neither.2 <- plm(form,
                     data=tst.neither,
                     index=c("firstent"),
                     model="within")

    outlist <- list(reciprocators=recip.2,
                    redistributors=redist.2,
                    neither=neither.2)
    return(outlist)
}


rf.reg <- sendThrough(predictedData=rf.preds,
                      STMData=out3)

svm.reg <- sendThrough(predictedData=svm.preds,
                       STMData=out3)

nb.reg <- sendThrough(predictedData=nb.preds,
                      STMData=out3)

glm.reg <- sendThrough(predictedData=glm.preds,
                       STMData=out3)

delg.reg <- sendThrough(predictedData=delg.preds2,
                        STMData=out3)

library(dotwhisker)
## Reciprocator Prevalence
recip.plot.all <- dwplot(list(RF=rf.reg[[1]],
            SVM=svm.reg[[1]],
            GLM=glm.reg[[1]],
            NB=nb.reg[[1]],
            Deleg=delg.reg[[1]]),
       vline = geom_vline(
           xintercept = 0,
           colour = "grey60",
           linetype = 2
           ))  %>% # plot line at zero _behind_coefs
    relabel_predictors(
        c(
            chinashock = "China Joins",
            FCshock = "2008 FC",
            Xishock = "Xi Ascends",
            Trumpshock = "Trump Elected",
            covidshock = "Covid",
            lag.M2T2 = "Deleg. Prev. Turn"
            )) +
    xlab("Coefficient Estimate") +
    ylab("") +
    ggtitle("Program Theme Prevalence After Shocks",
            subtitle="Reciprocator Delegate Paragraphs") +
    theme_bw()


## Redistributor Prevalence

redist.plot.all <- dwplot(list(RF=rf.reg[[2]],
            SVM=svm.reg[[2]],
            GLM=glm.reg[[2]],
            NB=nb.reg[[2]],
            Deleg=delg.reg[[2]]),
       vline = geom_vline(
           xintercept = 0,
           colour = "grey60",
           linetype = 2
           ))  %>% # plot line at zero _behind_coefs
    relabel_predictors(
        c(
            chinashock = "China Joins",
            FCshock = "2008 FC",
            Xishock = "Xi Ascends",
            Trumpshock = "Trump Elected",
            covidshock = "Covid",
            lag.M2T2 = "Deleg. Prev. Turn"
            )) +
    xlab("Coefficient Estimate") +
    ylab("") +
    ggtitle("Program Theme Prevalence After Shocks",
            subtitle="Redistributor Delegate Paragraphs") +
    theme_bw()


## Neither Reciprocator nor Redistributor

others.plot.all <- dwplot(list(RF=rf.reg[[3]],
            SVM=svm.reg[[3]],
            GLM=glm.reg[[3]],
            NB=nb.reg[[3]],
            Deleg=delg.reg[[3]]),
       vline = geom_vline(
           xintercept = 0,
           colour = "grey60",
           linetype = 2
           ))  %>% # plot line at zero _behind_coefs
    relabel_predictors(
        c(
            chinashock = "China Joins",
            FCshock = "2008 FC",
            Xishock = "Xi Ascends",
            Trumpshock = "Trump Elected",
            covidshock = "Covid",
            lag.M2T2 = "Deleg. Prev. Turn"
            )) +
    xlab("Coefficient Estimate") +
    ylab("") +
    ggtitle("Program Theme Prevalence After Shocks",
            subtitle="Other Delegate Paragraphs") +
    theme_bw()


ggsave(others.plot.all,
       file="CompareCoefsGroup3500TReplDeleg.png")
ggsave(recip.plot.all,
       file="CompareCoefsRecip500TReplDeleg.png")
ggsave(redist.plot.all,
       file="CompareCoefsRedist500TReplDeleg.png")

###%%%%%%%%%%%%%%%%%%
## Just RF and Delegate
##%%%%%%%%%%%%%%%%%%%


library(dotwhisker)
## Reciprocator Prevalence
recip.plot.bw <- dwplot(list(RF=rf.reg[[1]],
            Deleg=delg.reg[[1]]),
       vline = geom_vline(
           xintercept = 0,
           colour = "grey60",
           linetype = 2
           ))  %>% # plot line at zero _behind_coefs
    relabel_predictors(
        c(
            chinashock = "China Joins",
            FCshock = "2008 FC",
            Xishock = "Xi Ascends",
            Trumpshock = "Trump Elected",
            covidshock = "Covid",
            lag.M2T2 = "Deleg. Prev. Turn"
            )) +
        scale_colour_grey(
        start = .1,
        end = .5,
        name = "Classification",
        ##breaks = c(0, 1),
        labels = c("RF", "Delegate")
    )+
    xlab("Coefficient Estimate") +
    ylab("") +
    ggtitle("Program Theme Prevalence After Shocks",
            subtitle="Reciprocator Delegate Paragraphs") +
    theme_bw()+
     theme(legend.position='bottom')


## Redistributor Prevalence

redist.plot.bw <- dwplot(list(RF=rf.reg[[2]],
            Deleg=delg.reg[[2]]),
       vline = geom_vline(
           xintercept = 0,
           colour = "grey60",
           linetype = 2
           ))  %>% # plot line at zero _behind_coefs
    relabel_predictors(
        c(
            chinashock = "China Joins",
            FCshock = "2008 FC",
            Xishock = "Xi Ascends",
            Trumpshock = "Trump Elected",
            covidshock = "Covid",
            lag.M2T2 = "Deleg. Prev. Turn"
            )) +
        scale_colour_grey(
        start = .1,
        end = .5,
        name = "Classification",
        ##breaks = c(0, 1),
        labels = c("RF", "Delegate")
    )+
    xlab("Coefficient Estimate") +
    ylab("") +
    ggtitle("Program Theme Prevalence After Shocks",
            subtitle="Redistributor Delegate Paragraphs") +
    theme_bw()+
     theme(legend.position='bottom')


## Neither Reciprocator nor Redistributor

others.plot.bw <- dwplot(list(RF=rf.reg[[3]],
            Deleg=delg.reg[[3]]),
                      vline = geom_vline(
                          xintercept = 0,
                          colour = "grey60",
                          linetype = 2
                          ))  %>% # plot line at zero _behind_coefs
    relabel_predictors(
        c(
            chinashock = "China Joins",
            FCshock = "2008 FC",
            Xishock = "Xi Ascends",
            Trumpshock = "Trump Elected",
            covidshock = "Covid",
            lag.M2T2 = "Deleg. Prev. Turn"
            )) +
    scale_colour_grey(
        start = .1,
        end = .5,
        name = "Classification",
        ##breaks = c(0, 1),
        labels = c("RF", "Delegate")
    )+
    xlab("Coefficient Estimate") +
    ylab("") +
    ggtitle("Program Theme Prevalence After Shocks",
            subtitle="Other Delegate Paragraphs") +
    theme_bw()+
   theme(legend.position='bottom')



ggsave(others.plot.bw,
       file="BWCoefsGroupRD500TReplDeleg.png")
ggsave(recip.plot.bw,
       file="BWCoefsRecipRD500TReplDeleg.png")
ggsave(redist.plot.bw,
       file="BWCoefsRedistRD500TReplDeleg.png")


## All Frames on the Random Forest Classification:
rf.plot <- dwplot(list(Recip=rf.reg[[1]],
                       Redist=rf.reg[[2]],
                       Other=rf.reg[[3]]),
                  vline = geom_vline(
           xintercept = 0,
           colour = "grey60",
           linetype = 2
           ))  %>% # plot line at zero _behind_coefs
    relabel_predictors(
        c(
            chinashock = "China Joins",
            FCshock = "2008 FC",
            Xishock = "Xi Ascends",
            Trumpshock = "Trump Elected",
            covidshock = "Covid",
            lag.M2T2 = "Deleg. Prev. Turn"
            )) +
    scale_colour_grey(
        start = .1,
        end = .5,
        name = "Frame",
        ##breaks = c(0, 1),
        labels = c("Recip", "Redist", "Other/Neither")
    ) +
    xlab("Coefficient Estimate") +
    ylab("") +
    ggtitle("Program Theme Prevalence After Shocks",
            subtitle="Random Forest Classification, Delegate Paragraphs") +
    theme_bw()

rf.plot

ggsave(rf.plot,
       file="RFCoefficientsRD500TReplDeleg.png")



