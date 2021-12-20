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

ls()

colnames(out$meta)

head(out$meta$paratext)
head(out$meta$text)

out$meta$turn.lengths <- stringi::stri_count_words(
    out$meta$paratext) ## min: 1, median: 102; max: 4.6k

out$meta$turn.lengths.processed <- stringi::stri_count_words(
    out$meta$text) ## min: 1, median: 102; max: 4.6k

summary(out$meta$turn.lengths)

summary(out$meta$turn.lengths.processed)

## who have the long turns:
## top quartile:

long.turns <- out$meta[which(
    out$meta$turn.lengths >= 202),]

long.turns <- long.turns[order(long.turns$turn.lengths),]

dim(long.turns) ## 1302 x 27


colnames(long.turns)

## Longest ten:
long.turns[1292:1302,c("meetingno",
                   "firstent",
                   "turn.lengths")]


mod.out.2.meta <- out$meta
dim(mod.out.2.meta) ## 5229 x 26

## Summary lengths by country:
summary(out$meta[which(
    out$meta$firstent=="United States"),]$turn.lengths)

summary(out$meta[which(
    out$meta$firstent=="China"),]$turn.lengths)

summary(out$meta[which(
    out$meta$firstent=="European Union"),]$turn.lengths)

summary(out$meta[which(
    out$meta$firstent=="Egypt"),]$turn.lengths)

summary(out$meta[which(
    out$meta$firstent=="India"),]$turn.lengths)

#####################################
##### Subset: Topic Two-- "Programming" Frame
######################################

summary(mod.out.2) ## 

## Plot the model:
twoTopics <- c("Process", "Programs")

## Custom plot legend:
commas <- function(text){  
  paste(text[nchar(text)>0], collapse=", ")
}

label.t1 <- paste0("Process: ",
                   commas( ## paste frex
                       labelTopics(mod.out.2, n=5)$frex[1,]))

label.t2 <- paste0("Programs: ",
                   commas(
                       labelTopics(mod.out.2, n=5)$frex[2,]))

png(file="overallThemes.png")
plot.estimateEffect(prep.2,
                    model=mod.out.2,
                    covariate="meetingno",
                    main="CTD Corpus Dominant Themes",
                    topics=c(1:2), 
                    method="continuous",
                    xlab="Meeting Number",
                    labeltype="custom",
                    printlegend=FALSE,
                    n=5,
                    linecol=c("red", "darkblue")
                    )
legend("topright", legend=c(label.t1, label.t2),
       col=c("red", "darkblue"), lty=1)
dev.off()

## Extract paragraph-level topic assignments

theta2.out <- as.data.frame(round(mod.out.2$theta, 2))  ## round to two
colnames(theta2.out) <- gsub(pattern="V",
                             replace="M2.Topic",
                             x=as.character(colnames(
                                 theta2.out)))

theta2.out <- cbind(out$meta$pid, theta2.out)

#### Assign dominant topic
theta2.out$assignedtopic <- colnames(theta2.out[,2:3])[apply(
                                theta2.out[,2:3],1,which.max)] 

table(theta2.out$assignedtopic)

prop.table(table(theta2.out$assignedtopic))

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

colnames(mod.out.2.meta)
## Subset to each domain:

## Programs dominant paragraphs:
programsParas <- paraTopicsK2[which(
    paraTopicsK2$assignedtopic=="M2.Topic2"),]

dim(programsParas) ##3350 x 23

programsSpeakers <- as.data.frame(
    table(programsParas$firstent))
programsSpeakers <- programsSpeakers[order(
    programsSpeakers$Freq),]
tail(programsSpeakers)## Top speakers are:
## US (429); EU (248);
## India (229); Egypt (173);
## China (136), and Brazil (89)

### Iterate the topic model on subsets
## and add the shocks:

## Subset One: programselopment paragrahs

programsParas$cleanedtext <- gsub("[[:digit:]]", "",
                                  programsParas$paratext)

programsParas$cleanedtext <- gsub("[[:punct:]]", "",
                                  programsParas$paratext)

data2 <- corpus(programsParas, text_field = 'paratext')
docvars(data2)$text <- as.character(data2)

length(data2)

dat2 <- dfm(data2, stem = TRUE,
            remove = stopwords('english'),
            remove_punct = TRUE) %>%
    dfm_trim(min_termfreq = 2)


out <- convert(dat2, to = 'stm')

length(out)
attributes(out)

colnames(out$meta)

dim(out$meta)

round((table(out$meta$"income_level_iso3c")), 2)
round(prop.table(table(out$meta$"income_level_iso3c")), 2)

presenters <- as.data.frame(table(out$meta$firstent))
presenters <- presenters[order(presenters$Freq),]

dim(presenters) ##119

tail(presenters)
                            
#### Model sub-themes

form.base= ~s(meetingno)

#### Theme Cluster 2 Models:

set.seed(6889)
mod.out.theme2 <- stm(documents=out$documents,
                  vocab=out$vocab,
                  data=out$meta,
                 K=5, ## 
                  prevalence=form ,
               seed=61921)
 
prep.out.theme2 <- estimateEffect(c(1:5) ~s(meetingno),
                         mod.out.theme2,
                          metadata=out$meta,
                          documents=out$documents,
                          uncertainty=c("Global"))


summary(mod.out.theme2)
## re-ran in the afternoon; more label switchin!
## Topic 1: Commodities from LDCs
## Topic 2: LDC Market Access Provisions
## Topic 3: E-Commerce
## Topic 4: Technical Assistance
## Topic 5: Debates

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

