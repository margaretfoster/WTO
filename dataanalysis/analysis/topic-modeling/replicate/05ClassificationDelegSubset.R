## Use each model to estimate a DF with the tags
## at the end, compare the regression model
## for each version 
rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            {install.packages(lib,
                              repos='http://cran.rstudio.com/')}
        suppressMessages(library(lib,
                                 character.only=TRUE))}}


packs <- c('tidyr',
           'quanteda',
           'dplyr',
           'tidyverse',
           "readxl",
           'textrecipes',
           'rsample',
           "discrim")

engines <-  c('glmnet',
              "tidymodels",
              "naivebayes",
              "kernlab",
              "ranger")

loadPkg(c(packs, engines))

#############################
## Load csv of frame tags
#############################


load("twoTopicsAndSubSets-NoAdminSubset_CatFacRepl.Rdata")

ls()

rm(list=ls(pattern="mod*")) ## remove the K=2 models
rm(list=ls(pattern="prep*"))

dim(out$meta) ## 5115

## Grab "cleanedtext" field from meta:
## For the state delegation subset in out$meta: 
out$meta <- out$meta %>%
    left_join(x=out$meta,
              y= meta[,c("pid", "cleanedtext")],
              by=c("pid"))

tags <- read.csv("../parasTaggedFrames500.csv") ## 487 x 8

## Make sure that none of the tagged paragrpahs were
## removed in the de-dup:
ls() ## just want out and meta:

length(out$meta$pid) ## 5115
length(tags$pid) #3 487

## Make all column names to lowercase:
colnames(untagged) <- tolower(colnames(out$meta))
colnames(tags) <- tolower(colnames(tags))


tags[which(tags$pid== "case 193"), "pid"] <- 1385
tags$pid <- as.numeric(tags$pid)

## Keep only the intersection:
## (This makes it easier to go back and add
## (more if needed)

tags <- tags[which(tags$pid %in%out$meta$pid),]
dim(tags) ## 476

tagged.pids <- tags$pid
length(tagged.pids) ##476

## Prep for Prediction:
untagged <- out$meta[!(out$meta$pid %in% tagged.pids),]

dim(untagged) ##4639 x 16

colnames(untagged) <- tolower(colnames(untagged))
                         

## Merge tags and meta:
tagged <- merge(tags,
                out$meta,
                by.x="pid",
                by.y="pid",
                all.x=TRUE)

dim(tagged) ## (2/27: 476; just in delegate turns)

## not-needed:
tagged$numdate <- NULL
tagged$X.y <- NULL
tagged$X.x <- NULL

## Group the frame clusters:
## Framereciprocator: donor preferences + reciprocator
## Frameredist: recipient preferences + redistributor

tagged$frame <- "unknown"

tagged[tagged$dprefs==1 |
       tagged$recip==1,"frame"] <- "recip"

tagged[tagged$rprefs==1 |
       tagged$redist==1,"frame"] <- "redist"

table(tagged$frame) ## 84 reciprocator; 176 redist; 216 unknown

tagged$frame <- as.factor(tagged$frame)

## Training -test split
set.seed(2322) 

tagged.split <- initial_split(data = tagged,
                             strata = frame,
                             prop = .7)

tagged.split

tagged.train <- training(tagged.split)
tagged.test <- testing(tagged.split)

## ## K-Fold Cross-validation:
## ## 5 folds, given size of training set
## set.seed(22622)

## folds <- vfold_cv(tagged.train, v = 5)
## folds

##%%%%%%%%%%%%%%%%%%%%%%%
### Prep global settings for models
##%%%%%%%%%%%%%%%%%%%%%%%
## ID the columns for analysis + ID

wto.rec <- recipe(frame ~ cleanedtext + pid + year,
                  data = tagged.train) %>% 
    update_role(pid, year,
                new_role = "ID")  ## ID fields

## Clean and convert to dtm
wto.rec <- wto.rec %>%
    step_tokenize(cleanedtext) %>%
  step_stopwords(cleanedtext) %>%
    step_tokenfilter(cleanedtext) %>%
  step_tfidf(cleanedtext)

summary(wto.rec)

##%%%%%%%%%%%%%%%%%%
## Naive Bayes
##%%%%%%%%%%%%%%%%%

set.seed(2322)
nb.spec <- naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("naivebayes")

nb.spec

nb.workflow <- workflow() %>%
  add_recipe(wto.rec) %>%
  add_model(nb.spec)

nb.fit <- nb.workflow %>%
    fit(data = tagged.train)

nb.pred <- predict(nb.fit, tagged.test)

wto.nb.aug <- augment(nb.fit, tagged.test)
wto.nb.aug$frame <- as.factor(wto.nb.aug$frame)

## Add the X-Val Step next:
## nb.fit.xval <- nb.workflow %>%
##     fit(data = tagged.train)%>%
##     fit_resamples(folds,
##                   control = control_resamples(save_pred = TRUE))

## ## Verify results across folds:
## collect_metrics(nb.fit.xval)

## nb.rs.preds <- collect_predictions(nb.fit.xval)

##Plot XVal ROCs:

## nb.rs.preds %>%
##     group_by(id) %>%
##     roc_curve(truth = frame,
##               .pred_recip,
##               .pred_redist,
##               .pred_Unknown) %>%
##   autoplot() +
##   labs(
##     color = NULL,
##     title = "ROC Curve Naieve Bayes CrossValidation",
##     subtitle = "Each resample fold is shown in a different color"
##       )


## ## Heat Map To See How Well The Categories Separate:
## ## Answer: Not Well
## conf_mat_resampled(nb.fit.xval,
##                    tidy = FALSE) %>%
##     autoplot(type = "heatmap")

## ## Decide on model:

##%%%%%%%%%%%%%%%%%%
## Random Forest
##%%%%%%%%%%%%%%%%%%

## Structure:
set.seed(2322)

rf.spec <- rand_forest() %>%
  set_mode("classification") %>%
    set_engine("ranger")

rf.spec

rf.workflow <- workflow() %>%
  add_recipe(wto.rec) %>%
  add_model(rf.spec)

## train:
rf.fit <- rf.workflow %>%
    fit(data = tagged.train)

## predict:

rf.pred <- predict(rf.fit, tagged.test)

## Map into df 
wto.rf.aug <- augment(rf.fit, tagged.test)
wto.rf.aug$frame <- as.factor(wto.rf.aug$frame)

## RF: dominant model, so will predict using it:

##%%%%%%%%%%%%%%%%%%
## SVM
##%%%%%%%%%%%%%%%%%%

## structure:
set.seed(2322)

svm.spec <- svm_poly() %>%
  set_mode("classification") %>%
    set_engine("kernlab")

svm.workflow <- workflow() %>%
    add_recipe(wto.rec) %>%
    add_model(svm.spec)

svm.fit <- svm.workflow %>%
    fit(data = tagged.train)

## predict:

svm.pred <- predict(svm.fit, tagged.test)

wto.svm.aug <- augment(svm.fit, tagged.test)

wto.svm.aug$frame <- as.factor(wto.svm.aug$frame)

##%%%%%%%%%%%%%%%%%%
## Logistic Reg with LASSO
##%%%%%%%%%%%%%%%%%%

## structure:
set.seed(2322)

## penalty: mixture =1 is LASSO
## mixture =0 is ridge
glm.spec <- multinom_reg(mixture=double(1),
                         mode="classification",
                         engine= "glmnet",
                         penalty=0)

glm.workflow <- workflow() %>%
    add_recipe(wto.rec) %>%
    add_model(glm.spec)

glm.fit <- glm.workflow %>%
    fit(data = tagged.train)

## predict:
glm.pred <- predict(glm.fit, tagged.test)

wto.glm.aug <- augment(glm.fit, tagged.test)

wto.glm.aug$frame <- as.factor(wto.glm.aug$frame)

##%%%%%%%%%%%%%%%%%%%%%%%%%%
## Speaker-delegations
## From hand-coded
##%%%%%%%%%%%%%%%%%%%%%%%%%%
## Predicting based on delegations in training set
## on testing set

## Identify speakers in tagged redistributor paragraphs

redists <- as.data.frame(table(tagged.train[which(
    tagged.train$frame=="redist"),"firstent"]))

colnames(redists) <- c("deleg", "freq.redist")

## Speakers in reciprocator paragaphs
recip <- as.data.frame(table(tagged.train[which(
    tagged.train$frame=="recip"),"firstent"]))

colnames(recip) <- c("deleg", "freq.recip")

unknown <- as.data.frame(table(tagged.train[which(
    tagged.train$frame=="unknown"),"firstent"]))

colnames(unknown) <- c("deleg", "freq.unknown")

## union in redistributor vs reciprocator:
intersect(redists$Deleg, recip$Deleg) ## Overlap:
## China, EU, US

## Merge together; "Pred probability" as % in dominant category

delegations.twoway <- merge(recip,
                     redists,
                     by="deleg",
                     all=TRUE)

delegations.twoway[is.na(delegations.twoway)] <- 0

## Predict in test set based on top speaker in test set:
## Design "probabilities": Given that a paragraph is from a
## speaker in the reciprocator or reidstributor pool, what is the likelihood that the paragraph was a redist paragraph?
## (note, I'm not taking into account non-tagged paragraphs)
## so we know that it will be an over-estimate

allparas <- delegations.twoway$freq.recip +
    delegations.twoway$freq.redist

delegations.twoway$.pred_redist <- round(
    delegations.twoway$freq.redist/allparas, 3)

delegations.twoway$.pred_recip <- round(
    delegations.twoway$freq.recip/allparas, 3)

## Predict Class
delegations.twoway$.pred_class <- "unknown"

delegations.twoway[which(delegations.twoway$.pred_recip >
                         delegations.twoway$.pred_redist),
                   ".pred_class"] <- "recip"

delegations.twoway[which(delegations.twoway$.pred_recip <
                         delegations.twoway$.pred_redist),
                   ".pred_class"] <- "redist"

### Merge into the test set:
wto.key.aug <- tagged.test
colnames(wto.key.aug) <- tolower(colnames(wto.key.aug))

cols <- c("deleg", ".pred_redist",
          ".pred_recip", ".pred_class")

tst <- merge(wto.key.aug,
             delegations.twoway[,cols],
             by.x="firstent",
             by.y="deleg",
             all.x=TRUE)

##also the test set, for later:
tst2 <- merge(tagged.train,
             delegations.twoway[,cols],
             by.x="firstent",
             by.y="deleg",
              all.x=TRUE)

## And the full set for model comparisons:
wto.hand <- merge(out$meta,
                  delegations.twoway[,cols],
                  by.x="firstent",
                  by.y="deleg",
                  all.x=TRUE)
                  

tst[is.na(tst$.pred_class), ".pred_class"] <- "unknown"
tst2[is.na(tst2$.pred_class), ".pred_class"] <- "unknown"
wto.hand[is.na(wto.hand$.pred_class), ".pred_class"] <- "unknown"

## Expected probabily of "unknown" = 100% if not in the
## recip/redist delegate groups. B/c this is the residual

tst[is.na(tst$.pred_class), ".pred_class"] <- "unknown"

tst$.pred_unknown <-0
tst[which(tst$.pred_class=="unknown"),
    ".pred_unknown"] <- 1

tst[is.na(tst$.pred_redist), ".pred_redist"] <- 0
tst[is.na(tst$.pred_recip), ".pred_recip"] <- 0


tst[,c("firstent", "frame", ".pred_class",
       ".pred_redist", ".pred_recip",
       ".pred_unknown")]

## The training set:
## Create predicted unknown:
tst2[is.na(tst2$.pred_class), ".pred_class"] <- "unknown"
tst2$.pred_unknown <-0
tst2[which(tst2$.pred_class=="unknown"),
    ".pred_unknown"] <- 1

tst2[is.na(tst2$.pred_redist), ".pred_redist"] <- 0
tst2[is.na(tst2$.pred_recip), ".pred_recip"] <- 0

## Scale to full data:

wto.hand[is.na(wto.hand$.pred_class),
         ".pred_class"] <- "unknown"
wto.hand$.pred_unknown <-0
wto.hand[which(wto.hand$.pred_class=="unknown"),
    ".pred_unknown"] <- 1

wto.hand[is.na(wto.hand$.pred_redist),
         ".pred_redist"] <- 0
wto.hand[is.na(wto.hand$.pred_recip),
         ".pred_recip"] <- 0

## write entire hand-tagged + predicted:
cols2 <- c("pid",
           "frame",
           ".pred_class",
           ".pred_redist",
           ".pred_recip",
           ".pred_unknown")

tst.out <- rbind(tst[,cols2],
                 tst2[, cols2])

##%%%%%%%%%%%%%%%%%%%%
## Scale Predictions
#%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Scale prediction from RF Model:
## Whole data:

## RF
rf.pred.all <- predict(rf.fit, untagged)
wto.rf.aug <- augment(rf.fit, untagged)

dim(wto.rf.aug) ## 7978 x 20 ## => 8.4k - the tagged

## GLM (glm.fit)
glm.pred.all <- predict(glm.fit, untagged)
wto.glm.aug <- augment(glm.fit, untagged)

## NB
nb.pred.all <- predict(nb.fit, untagged)
wto.nb.aug <- augment(nb.fit, untagged)

## SVM
svm.pred.all <- predict(svm.fit, untagged)
wto.svm.aug <- augment(svm.fit, untagged)

## By delegations -> wto.hand above
save(wto.glm.aug,
     wto.nb.aug,
     wto.rf.aug,
     wto.svm.aug,
     wto.hand,
     tagged,
     file="predicted-models500ReplDelegSubset.Rdata")
