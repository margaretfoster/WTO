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
           'stargazer')

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

mod.out.2.meta <- out$meta
dim(mod.out.2.meta) ## 5229 x 26

ls()

load(file="themeSubsetTheme2.Rdata")

summary(length(mod.out.2.meta$paratext))
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

##%%%%%%%%%%%%%%%%%%%%%%
## Within sub-theme frame analysis:
##%%%%%%%%%%%%%%%%%%%%%

## First, list of Redistributors
## and Reciprocactors

reciprocators <- c("Switzerland", "Trinidad and Tobago",
                   "European Union", "Japan", "United States",
                   "Canada")


redistributors <- c("El Salvador","Argentina",
                    "Cote DIvoire",
                    "Egypt",
                    "Poland",
                    "Brazil",
                    "Mexico",
                    "Jamaica",
                    "Bangladesh",
                    "Uganda",
                    "China",
                    "Cambodia",
                    "Lesotho",
                    "Malaysia",
                    "Guatemala",
                    "Bolivia",
                    "Zambia",
                    "Dominica",
                    "Morocco",
                    "Fiji",
                    "Cuba",
                    "Sri Lanka",
                    "Kenya",
                    "Uruguay",
                    "India")


## Topic 3: Policy

colnames(out3)

out3$redistributors <- 0
out3[which(out3$firstent %in% redistributors), "redistributors"] <- 1

out3$reciprocators <- 0
out3[which(out3$firstent %in% reciprocators), "reciprocators"] <- 1

### And so what we want is Topic 2 topic proportion
## ~ shock periods
## expectation: will increase in shock periods



###%%%%%%%%%%%%%%%%%%
## Summary Stats
##%%%%%%%%%%%%%%%%%%%
table(out3$assignedtopic.y) ## Topic 3 is dominant (1382)
table(out3$assignedtopic.x)## 1715 topic 1; 3514 topic 2

table(out3$redistributors) # 3342 no; 1886 yes
table(out3$reciprocators) ## 3548 no; 1681 yes

## Descriptive table for draft
t1 <- as.data.frame(table(
    out3[which(out3$reciprocators==1),]$firstent))

t2 <- table(out3[which(out3$redistributors==1),]$firstent)

stargazer(t1,
          type="html",
          summary=FALSE,
          title="Reciprocator Delegations",
          digits=1,
          column.labels=c("Delegation", "Presentations"),
          out="t1.html")

## Check out the topic proportions:
summary(out3[which(out3$reciprocators==1),]$M2.Topic2)
summary(out3[which(out3$redistributors==1),]$M2.Topic2)

## T-test on the two sections:
recip.t2 <- out3[which(out3$reciprocators==1),]$M2.Topic2
redist.t2 <- out3[which(out3$redistributors==1),]$M2.Topic2

## though, this isn't the test that we actually want
## we want a test of affect in framing, not frequency
t.test(recip.t2, redist.t2)

##Though, a t-test on means in the Topic 2 subtopics
## is probably worthwhile, since we think that the
## redistributors talk more about programs and the
## reciprocators talk more about process:

recip.t2.3 <- out3[which(out3$reciprocators==1),]$MT2.Topic3
redist.t2.3 <- out3[which(out3$redistributors==1),]$MT2.Topic3
t.test(recip.t2.3, redist.t2.3) ## means are basically identical....


### Need topic proportion lag:
## M2.Topic1; M2.Topic2
## grouped by firstent, then year, then meeting, then paranum

library(dplyr)

tst <- out3 %>%
    arrange(firstent, meetingno, paranum)  %>%
    mutate(lag.M2T2 = lag(M2.Topic2)) 

head(tst[which(tst$firstent=="India"),
          c("firstent", "meetingno",
            "paranum", "pid",
            "M2.Topic2", "lag.M2T2")])

head(tst[which(tst$firstent=="Egypt"),
          c("firstent", "meetingno",
            "paranum", "pid",
            "M2.Topic2", "lag.M2T2")])


tail(tst[which(tst$firstent=="Egypt"),
          c("firstent", "meetingno",
            "paranum", "pid",
            "M2.Topic2", "lag.M2T2")])

dim(out3)

## The substantive claim is that


library(sandwich)
library(plm)

colnames(tst)
## Model:
## Topic proportion ~ Shock periods 1:5 + lag

tst.recip <- tst[which(tst$reciprocators==1),]
tst.redib <- tst[which(tst$redistributors==1),]

tst.neither <- tst[which(tst$redistributors==0 &
                         tst$reciprocators==0),]

dim(tst.recip) ## 1681
dim(tst.redib) ## 1886
dim(tst.neither) ## 1662 
## form:

form <- as.formula("M2.Topic2 ~ chinashock + FCshock +
Xishock + Trumpshock + covidshock + lag.M2T2")

reciprocators <- lm(formula= form,
                    data=tst.recip,)

redist <- lm(formula= form,
                    data=tst.redib,)

neither <- lm(formula=form,
              data=tst.neither)

summary(reciprocators)
summary(redist)
summary(neither)

## Speaker fixed effects:

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

summary(reciprocators)
summary(recip.2)

stargazer(recip.2, redist.2, neither.2,
          type="html",
          dep.var.labels=c("Proportion of Delegate Presentations in Development Theme"),
          covariate.labels=c("China 'Shock'",
              "Financial Crisis 'Shock'",
              "Xi 'Shock'",
              "Trump 'Shock'",
              "Covid 'Shock'",
              "Previous Turn"),
          title="Results",
          column.labels = c("Reciprocators",
              "Redistributors",
              "Neither"),
          out="models.html")
