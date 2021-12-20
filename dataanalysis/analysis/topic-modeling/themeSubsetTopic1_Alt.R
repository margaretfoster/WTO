### Preliminary analysis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 'pdftools',
           'tidyr', 'quanteda',
           "plm", 'sandwich',
           "stringi")

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
load("twoTopicsAndSubSets-NoAdminSubset_Alt.Rdata")

## For summary statistics;
## see the themSubsetTopic2_Alt.R script

#####################################
##### Subset: Topic One-- "Process" Frame
######################################
summary(mod.out.2) ## 
mod.out.2.meta <- out$meta

## Extract paragraph-level topic assignments

theta2.out <- as.data.frame(round(mod.out.2$theta, 2))  ## round to two
colnames(theta2.out) <- gsub(pattern="V",
                             replace="M2.Topic",
                             x=as.character(colnames(
                                 theta2.out)))

theta2.out <- cbind(out$meta$pid, theta2.out)

#### Assign dominant topic:
theta2.out$assignedtopic <- colnames(theta2.out[,2:3])[apply(
                                theta2.out[,2:3],1,which.max)] 

table(theta2.out$assignedtopic)

round(prop.table(table(theta2.out$assignedtopic)),2)

head(theta2.out)

paraTopicsK2 <- merge(x =mod.out.2.meta,
                    y=theta2.out,
                    by.x="pid",
                    by.y="out$meta$pid")

dim(paraTopicsK2) ## 5228 x 19

colnames(paraTopicsK2)

head(paraTopicsK2)

############################
## Identify the theorized shocks
## In the second theme (Programming)
############################

## Subset to Topic 1:

## Process-dominant paragraphs:
processParas <- paraTopicsK2[which(
    paraTopicsK2$assignedtopic=="M2.Topic1"),]

dim(processParas) ##1715 x 29


### Iterate the topic model on subsets
## and add the shocks:

## Subset One: Clean the Process paragraphs

processParas$cleanedtext <- gsub("[[:digit:]]", "",
                                  processParas$paratext)

processParas$cleanedtext <- gsub("[[:punct:]]", "",
                                  processParas$paratext)

data2 <- corpus(processParas, text_field = 'paratext')
docvars(data2)$text <- as.character(data2)

length(data2)

dat2 <- dfm(data2, stem = TRUE,
            remove = stopwords('english'),
            remove_punct = TRUE) %>%
    dfm_trim(min_termfreq = 2)

out <- convert(dat2, to = 'stm')

round((table(out$meta$"income_level_iso3c")), 2)
round(prop.table(table(out$meta$"income_level_iso3c")), 2)

presenters <- as.data.frame(table(out$meta$firstent))
presenters <- presenters[order(presenters$Freq),]

dim(presenters) ##77
                            
#### Model sub-themes

recip <- c("European Union", "Canada",
           "Japan", "Switzerland", "United States")

redist <- c("Bangladesh","China", "Egypt", "India", 
            "Morocco", "Uruguay")


faction1 <- c("European Union", "Canada",
              "United States")

faction2 <- c("China", "Egypt", "India")


out$meta$cat <- "Other"
out$meta[which(
    out$meta$firstent %in% faction1),
         "cat"] <- "US-EU-Can"

out$meta[which(
    out$meta$firstent %in% faction2),
         "cat"] <- "China-Egypt-India"

out$meta$cat <- as.factor(out$meta$cat)

form.base= ~s(meetingno)+ cat

#### Theme Cluster Models, Topic 1:

set.seed(6889)

mod.out.theme1 <- stm(documents=out$documents,
                  vocab=out$vocab,
                  data=out$meta,
                 K=5, ## 
                  prevalence=form.base,
               seed=61921)

prep.out.theme1 <- estimateEffect(c(1:5) ~s(meetingno)+ cat,
                         mod.out.theme1,
                          metadata=out$meta,
                          documents=out$documents,
                          uncertainty=c("Global"))

summary(mod.out.theme1)

## 10:

set.seed(6889)
mod.out.theme10 <- stm(documents=out$documents,
                  vocab=out$vocab,
                  data=out$meta,
                 K=10, ## 
                  prevalence=form.base ,
               seed=61921)
 
prep.out.theme10 <- estimateEffect(c(1:10) ~s(meetingno)+cat,
                         mod.out.theme10,
                          metadata=out$meta,
                          documents=out$documents,
                          uncertainty=c("Global"))

attributes(mod.out.theme1)
names(mod.out.theme1$settings)
mod.out.theme1$settings$covariates

## make.dt() combines doc-topic loadings (theta) +  metadata 
topicprop<-make.dt(mod.out.theme1, out$meta)

class(topicprop)
colnames(topicprop) ## topic percentages + metadata 

dim(topicprop)

save.image(file="ProcessSubSetstm.Rdata")

## Topic 1: Agenda Items
## Topic 2: OPEC and Observers
## Topic 3: Regional Trade Agreements
## Topic 4: LAIA
## Topic 5: Duties and Tarrifs


topicNames <- c( "LDC Market Access",
                "Technical Assistance",
                "Policy Debates",
                "Commodities",
                "Training")


pdf(file="Theme2Overall.pdf")
par(bty="n",col="grey40",lwd=5)
plot.STM(mod.out.theme2,
         type="summary",
         custom.labels="",
         main="Subtopics in Programs Transcript Theme",
         topic.names=topicNames)
dev.off()

png("devSubTheme.png")
par(mfrow=c(3,1))
plot(prep.out.theme2,
     covariate="meetingno",
     method = "continuous",
     topics = c(1,2),
     model = mod.out.theme2,
     printlegend = TRUE,
     labeltype="frex",
     linecol=c("red", "darkblue"),
     ## topic.names=topicNames,
     xlab = "Meeting Number (1995-2020)")

plot(prep.out.theme2,
     covariate="meetingno",
     method = "continuous",
     topics = c(3),
     model = mod.out.theme2,
     printlegend = TRUE,
     labeltype="frex",
     linecol=c("darkgreen"),
     ## topic.names=topicNames,
     xlab = "Meeting Number (1995-2020)")

plot(prep.out.theme2,
     covariate="meetingno",
     method = "continuous",
     topics = c(4,5),
     model = mod.out.theme2,
     printlegend = TRUE,
     labeltype="frex",
     linecol=c("darkgray", "goldenrod"),
    ## topic.names=topicNames,
     xlab = "Meeting Number (1995-2020)")   
 
dev.off()

##%%%%%%%%%%%%%%%%%%%%%%%%
## Ascribe topics to paragraphs

mod2.out <- as.data.frame(round(mod.out.theme2$theta, 2))  ## round to two                                                      

head(mod2.out)

colnames(mod2.out) <- gsub(pattern="V",
                           replace="MT2.Topic",
                           x=as.character(colnames(mod2.out)))

head(mod2.out)

mod2.out <- cbind(out$meta$pid, mod2.out)

mod2.out$assignedtopic <- colnames(mod2.out[,2:6])[apply(
                                mod2.out[,2:6 ],1,which.max)]

table(mod2.out$assignedtopic)
round(prop.table(table(mod2.out$assignedtopic)), 2)

head(mod2.out)

### merge mod2.out back into the out$meta:

colnames(paraTopicsK2)

out3 <- merge(paraTopicsK2, ## Themes 1 & 2 proportions
              mod2.out, ## Theme 2 subset topics
              by.x="pid",
              by.y="out$meta$pid",
              all.x=TRUE)

dim(out3) ## 5229 x 35; full set of paragraphs


write.csv(mod2.out,
         file="withinTheme2.csv")

save.image(file="themeSubsetTheme2.Rdata")


## ##%%%%%%%%%%%%%%%%%%%%%%
## ## Within sub-theme frame analysis:
## ##%%%%%%%%%%%%%%%%%%%%%

## ## First, list of Redistributors
## ## and Reciprocactors

## reciprocators <- c("Switzerland", "Trinidad and Tobago",
##                    "European Union", "Japan", "United States",
##                    "Canada")


## redistributors <- c("El Salvador","Argentina",
##                     "Cote DIvoire",
##                     "Egypt",
##                     "Poland",
##                     "Brazil",
##                     "Mexico",
##                     "Jamaica",
##                     "Bangladesh",
##                     "Uganda",
##                     "China",
##                     "Cambodia",
##                     "Lesotho",
##                     "Malaysia",
##                     "Guatemala",
##                     "Bolivia",
##                     "Zambia",
##                     "Dominica",
##                     "Morocco",
##                     "Fiji",
##                     "Cuba",
##                     "Sri Lanka",
##                     "Kenya",
##                     "Uruguay",
##                     "India")


## ## Topic 3: Policy

## colnames(out3)

## out3$redistributors <- 0
## out3[which(out3$firstent %in% redistributors), "redistributors"] <- 1

## out3$reciprocators <- 0
## out3[which(out3$firstent %in% reciprocators), "reciprocators"] <- 1

## ### And so what we want is Topic 2 topic proportion
## ## ~ shock periods
## ## expectation: will increase in shock periods



## ###%%%%%%%%%%%%%%%%%%
## ## Summary Stats
## ##%%%%%%%%%%%%%%%%%%%
## table(out3$assignedtopic.y) ## Topic 3 is dominant (1382)
## table(out3$assignedtopic.x)## 1715 topic 1; 3514 topic 2

## table(out3$redistributors) # 2310 no; 1195 yes
## table(out3$reciprocators) ## 2499 no; 1014 yes

## table(out3[which(out3$reciprocators==1),]$firstent)

## table(out3[which(out3$redistributors==1),]$firstent)

## ## Check out the topic proportions:
## summary(out3[which(out3$reciprocators==1),]$M2.Topic2)
## summary(out3[which(out3$redistributors==1),]$M2.Topic2)

## ## T-test on the two sections:
## recip.t2 <- out3[which(out3$reciprocators==1),]$M2.Topic2
## redist.t2 <- out3[which(out3$redistributors==1),]$M2.Topic2

## ## though, this isn't the test that we actually want
## ## we want a test of affect in framing, not frequency
## t.test(recip.t2, redist.t2)

## ##Though, a t-test on means in the Topic 2 subtopics
## ## is probably worthwhile, since we think that the
## ## redistributors talk more about programs and the
## ## reciprocators talk more about process:

## recip.t2.3 <- out3[which(out3$reciprocators==1),]$MT2.Topic3
## redist.t2.3 <- out3[which(out3$redistributors==1),]$MT2.Topic3
## t.test(recip.t2.3, redist.t2.3) ## means are basically identical....


## ### Need topic proportion lag:
## ## M2.Topic1; M2.Topic2
## ## grouped by firstent, then year, then meeting, then paranum

## library(dplyr)

## tst <- out3 %>%
##     arrange(firstent, meetingno, paranum)  %>%
##     mutate(lag.M2T2 = lag(M2.Topic2)) 

## head(tst[which(tst$firstent=="India"),
##           c("firstent", "meetingno",
##             "paranum", "pid",
##             "M2.Topic2", "lag.M2T2")])

## head(tst[which(tst$firstent=="Egypt"),
##           c("firstent", "meetingno",
##             "paranum", "pid",
##             "M2.Topic2", "lag.M2T2")])


## tail(tst[which(tst$firstent=="Egypt"),
##           c("firstent", "meetingno",
##             "paranum", "pid",
##             "M2.Topic2", "lag.M2T2")])

## dim(out3)

## ## The substantive claim is that


## library(sandwich)
## library(plm)

## colnames(tst)
## ## Model:
## ## Topic proportion ~ Shock periods 1:5 + lag

## tst.recip <- tst[which(tst$reciprocators==1),]
## tst.redib <- tst[which(tst$redistributors==1),]

## tst.neither <- tst[which(tst$redistributors==0 &
##                          tst$reciprocators==0),]

## dim(tst.recip) ## 1681
## dim(tst.redib) ## 1886
## dim(tst.neither) ## 1662 
## ## form:

## form <- as.formula("M2.Topic2 ~ chinashock + FCshock +
## Xishock + Trumpshock + covidshock + lag.M2T2")

## reciprocators <- lm(formula= form,
##                     data=tst.recip,)

## redist <- lm(formula= form,
##                     data=tst.redib,)

## neither <- lm(formula=form,
##               data=tst.neither)

## summary(reciprocators)
## summary(redist)
## summary(neither)

## ## Speaker fixed effects:

## recip.2 <- plm(form,
##                data=tst.recip,
##                index=c("firstent"),
##                model="within")


## redist.2 <- plm(form,
##                data=tst.recip,
##                index=c("firstent"),
##                model="within")

## summary(reciprocators)
## summary(recip.2)

