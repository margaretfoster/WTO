### Preliminary analysis of trade and development
## Compare if frame regression model would give different
## results for different classification algorithems

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
load(file="predicted-models.Rdata")

## Hand-Tagged:
tags.hand <- read.csv("wto-hand-class.csv")

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

delg.preds <- rbind(wto.hand[,cols],
                       tags.hand[,cols] )


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
redist.plot <- dwplot(list(RF=rf.reg[[1]],
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

recip.plot <- dwplot(list(RF=rf.reg[[2]],
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
       file="CompareCoefsGroup3.pdf")
ggsave(recip.plot,
       file="CompareCoefsRecip.pdf")
ggsave(redist.plot,
       file="CompareCoefsRedist.pdf")

## By Hand:
       
gg <- rbind(cbind(as.data.frame(
    summary(svm.reg[[1]])$coefficients),
                  model="SVM",
                  sector="Reciprocators"),
            cbind(as.data.frame(
                summary(rf.reg[[1]])$coefficients),
                  model="RF",
                  sector="Reciprocators"),
            cbind(as.data.frame(
                summary(nb.reg[[1]])$coefficients),
                  model="NB",
                  sector="Reciprocators"),
            cbind(as.data.frame(
                summary(glm.reg[[1]])$coefficients),
                  model="GLM",
                  sector="Reciprocators"),
            cbind(as.data.frame(
                summary(delg.reg[[1]])$coefficients),
                  model="Delg",
                  sector="Reciprocators"),
            cbind(as.data.frame(
    summary(svm.reg[[2]])$coefficients),
                  model="SVM",
                  sector="Redistributors"),
            cbind(as.data.frame(
                summary(rf.reg[[2]])$coefficients),
                  model="RF",
                  sector="Redistributors"),
            cbind(as.data.frame(
                summary(nb.reg[[2]])$coefficients),
                  model="NB",
                  sector="Redistributors"),
            cbind(as.data.frame(
                summary(glm.reg[[2]])$coefficients),
                  model="GLM",
                  sector="Redistributors"),
            cbind(as.data.frame(
                summary(delg.reg[[2]])$coefficients),
                  model="Delg",
                  sector="Redistributors"),
            cbind(as.data.frame(
    summary(svm.reg[[3]])$coefficients),
                  model="SVM",
                  sector="Neither"),
            cbind(as.data.frame(
                summary(rf.reg[[3]])$coefficients),
                  model="RF",
                  sector="Neither"),
            cbind(as.data.frame(
                summary(nb.reg[[3]])$coefficients),
                  model="NB",
                  sector="Neither"),
            cbind(as.data.frame(
                summary(glm.reg[[3]])$coefficients),
                  model="GLM",
                  sector="Neither"),
            cbind(as.data.frame(
                summary(delg.reg[[3]])$coefficients),
                  model="Delg",
                  sector="Neither"))
            
dim(gg) ## 90x6

##Dot and whisker grouped by model; facet wrap by sector


    ###%%%%%%%%%%%%%%%%%%
    ## Summary Stats
    ##%%%%%%%%%%%%%%%%%%%
    table(out4$assignedtopic.y) ## Topic 3 is dominant (1382)
    table(out4$assignedtopic.x)## 1715 topic 1; 3514 topic 2
    
    table(out4$redistributors) # 3958 no; 1271 yes [this is not super different from the previous list form]
table(out4$reciprocators) ## 5119 no; 110 yes [This is about 10% of the delegation classification form]

## Descriptive table for draft
t1 <- as.data.frame(table(
    out4[which(out4$reciprocators==1),]$firstent))

t2 <- table(out4[which(out4$redistributors==1),]$firstent)

stargazer(t1,
          type="html",
          summary=FALSE,
          title="Reciprocator Delegations",
          digits=1,
          column.labels=c("Delegation", "Presentations"),
          out="t1-update.html")

## Check out the topic proportions:
summary(out4[which(out4$reciprocators==1),]$M2.Topic2)
summary(out4[which(out4$redistributors==1),]$M2.Topic2)

## T-test on the two sections:
recip.t2 <- out4[which(out4$reciprocators==1),]$M2.Topic2
redist.t2 <- out4[which(out4$redistributors==1),]$M2.Topic2

## though, this isn't the test that we actually want
## we want a test of affect in framing, not frequency
t.test(recip.t2, redist.t2)

##Though, a t-test on means in the Topic 2 subtopics
## is probably worthwhile, since we think that the
## redistributors talk more about programs and the
## reciprocators talk more about process:

recip.t2.3 <- out4[which(out4$reciprocators==1),]$MT2.Topic3
redist.t2.3 <- out4[which(out4$redistributors==1),]$MT2.Topic3
t.test(recip.t2.3, redist.t2.3) ## means are basically identical....



