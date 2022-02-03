## Measure 

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


##install.packages("randomForest",
##                 dependencies=TRUE,
##                 repos='http://cran.us.r-project.org')

packs <- c('tidyr',
           'quanteda',
           'dplyr',
           'tidyverse',
           "readxl",
           'textrecipes',
           'rsample',
           "discrim",
           "tidymodels",
           "naivebayes",
           "kernlab")

loadPkg(c(packs))

#############################
## Load csv of frame tags
#############################
load("~/Dropbox/WTO-Data/rdatas/processedTextforSTM.Rdata")

tags <- read.csv("parasTaggedFrames.csv")
 ## and load data

ls()
head(meta)

## Merge tags and meta:

tagged <- merge(tags,
                out$meta,
                by.x="PID",
                by.y="pid",
                all.x=TRUE)

dim(tagged) ## 239 x 26

head(tagged)

colnames(tagged)
## not-needed:
tagged$numdate <- NULL
tagged$X.y <- NULL
tagged$X.x <- NULL

## Group the frame clusters:
## FrameR: donor preferences + reciprocator
## FrameRedist: recipient preferences + redistributor

tagged$Frame <- "Unknown"

tagged[tagged$dprefs==1 |
       tagged$recip==1,"Frame"] <- "Recip"

tagged[tagged$rprefs==1 |
       tagged$redist==1,"Frame"] <- "Redist"

table(tagged$Frame) ## 39 reciprocator; 103 redist; 97 unknown

tagged$Frame <- as.factor(tagged$Frame)

## Training -test split
tagged.split <- initial_split(data = tagged,
                             strata = Frame,
                             prop = .7)

tagged.split

tagged.train <- training(tagged.split)
tagged.test <- testing(tagged.split)

### Processing data
## Using "tidy" recipe format

## ID the columns for analysis + ID
wto.rec <- recipe(Frame ~ cleanedtext + PID + year,
                  data = tagged.train) %>% 
    update_role(PID, year,
                new_role = "ID")  ## ID fields

wto.rec <- wto.rec %>%
    step_tokenize(cleanedtext) %>%
  step_stopwords(cleanedtext) %>%
    step_tokenfilter(cleanedtext) %>%
  step_tfidf(cleanedtext)

summary(wto.rec)

        ## Naive Bayes
 
nb.spec <- naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("naivebayes")

nb.spec

nb.workflow <- workflow() %>%
  add_recipe(wto.rec) %>%
  add_model(nb.spec)

nb.workflow %>%
    fit(data = tagged.train)

## Random Forest

rf.spec <- rand_forest() %>%
  set_mode("classification") %>%
    set_engine("randomForest")

rf.spec

rf.workflow <- workflow() %>%
  add_recipe(wto.rec) %>%
  add_model(rf.spec)

rf.workflow %>%
    fit(data = tagged.train)

## SVM

svm.spec <- svm_poly() %>%
  set_mode("classification") %>%
    set_engine("kernlab")

svm.workflow <- workflow() %>%
    add_recipe(wto.rec) %>%
    add_model(svm.spec)

svm.workflow %>%
    fit(data = tagged.train)


## Logit Regression with LASSO

