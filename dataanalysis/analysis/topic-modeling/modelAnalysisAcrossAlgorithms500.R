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
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}

packs <- c('tm', 'stm', 'pdftools',
           'tidyr', 'quanteda',
           "plm", 'sandwich',
           'stargazer',
           'dotwhisker')

loadPkg(packs)


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
#############################

load("twoTopicsAndSubSets-NoAdminSubset_CatFac.Rdata")
load(file="themeSubsetTheme2.Rdata")
load(file="predicted-models500.Rdata")

## Hand-Tagged:
tags.hand <- read.csv("wto-hand-class500.csv")

colnames(tags.hand)
tags.hand$X <- NULL
table(tags.hand$.pred_class)

## Predicted models are:
## wto.hand; wto.nb.aug; wto.rf.aug
## wto.svm.aug

cols <- c("PID", ".pred_class",
          ".pred_Recip", ".pred_Redist",
          ".pred_Unknown")

rf.preds <- rbind(wto.rf.aug[,cols],
                       tags.hand[,cols])

nb.preds <- rbind(wto.nb.aug[,cols],
                       tags.hand[,cols])

svm.preds <- rbind(wto.svm.aug[,cols],
                       tags.hand[,cols])

glm.preds <- rbind(wto.glm.aug[,cols],
                   tags.hand[,cols])

delg.preds <- wto.hand ## already added together

## Sanity check-- should all be the same & 8854
dim(rf.preds)
dim(nb.preds)
dim(svm.preds)
dim(glm.preds)
dim(delg.preds)

##%%%%%%%%%%%%%%%%%%%%%%%%
## Attach topics to paragraphs

mod2.out <- as.data.frame(round(
    mod.out.theme2$theta, 2))  ## round to two

head(mod2.out)

colnames(mod2.out) <- gsub(pattern="V",
                           replace="MT2.Topic",
                           x=as.character(
                               colnames(mod2.out)))

head(mod2.out)

mod2.out <- cbind(out$meta$pid, mod2.out)

mod2.out$assignedtopic <- colnames(mod2.out[,2:6])[apply(
    mod2.out[,2:6 ],1,which.max)]

table(mod2.out$assignedtopic)
round(prop.table(table(mod2.out$assignedtopic)), 2)

out3 <- merge(paraTopicsK2, ## Themes 1 & 2 proportions
              mod2.out, ## Theme 2 subset topics
              by.x="pid",
              by.y="out$meta$pid",
              all.x=TRUE)

## Attach the predicted and hand-tagged frames
dim(out3) ## 5229 x 35; full set of paragraphs


set.seed(2622)

sendThrough <- function(predictedData, STMData){

    set.seed(2622)
    library('dplyr')
    library('sandwich')
    library('plm')
    
    out4 <- merge(STMData, ## model predictions
                  predictedData,
                  by.x="pid",
                  by.y="PID",
                  all.x=TRUE)
        
    out4$redistributors <- 0
    out4[which(out4$.pred_class=="Redist"),
         "redistributors"] <- 1
    
    out4$reciprocators<- 0
    out4[which(out4$.pred_class=="Recip"),
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

delg.reg <- sendThrough(predictedData=delg.preds,
                        STMData=out3)

library(dotwhisker)

## Reciprocator Prevalence
recip.plot <- dwplot(list(RF=rf.reg[[1]],
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
    ggtitle("Frame Prevalence After Shocks",
            subtitle="Reciprocator Paragraphs") +
    theme_bw()


## Redistributor Prevalence

redist.plot <- dwplot(list(RF=rf.reg[[2]],
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
    ggtitle("Frame Prevalence After Shocks",
            subtitle="Redistributor Paragraphs") +
    theme_bw()


## Neither Reciprocator nor Redistributor

others.plot <- dwplot(list(RF=rf.reg[[3]],
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
    ggtitle("Frame Prevalence After Shocks",
            subtitle="Other Paragraphs") +
    theme_bw()


ggsave(others.plot,
       file="CompareCoefsGroup3500T.pdf")
ggsave(recip.plot,
       file="CompareCoefsRecip500T.pdf")
ggsave(redist.plot,
       file="CompareCoefsRedist500T.pdf")


###%%%%%%%%%%%%%%%%%%
## Summary Stats
##%%%%%%%%%%%%%%%%%%%


