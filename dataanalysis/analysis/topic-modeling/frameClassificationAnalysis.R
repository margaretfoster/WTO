  ## Measure 

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


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
set.seed(2322) 

tagged.split <- initial_split(data = tagged,
                             strata = Frame,
                             prop = .7)

tagged.split

tagged.train <- training(tagged.split)
tagged.test <- testing(tagged.split)

##%%%%%%%%%%%%%%%%%%%%%%%
### Prep global settings for models
##%%%%%%%%%%%%%%%%%%%%%%%
## ID the columns for analysis + ID

wto.rec <- recipe(Frame ~ cleanedtext + PID + year,
                  data = tagged.train) %>% 
    update_role(PID, year,
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
wto.nb.aug$Frame <- as.factor(wto.nb.aug$Frame)

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
wto.rf.aug$Frame <- as.factor(wto.rf.aug$Frame)

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

wto.svm.aug$Frame <- as.factor(wto.svm.aug$Frame)

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

wto.glm.aug$Frame <- as.factor(wto.glm.aug$Frame)


## %%%%%%%%%%%%%%%%%
## ROC
## %%%%%%%%%%%%%%%%%

nb.roc <- wto.nb.aug %>% 
    roc_curve(truth = Frame,
              .pred_Recip, .pred_Redist,.pred_Unknown,
              event_level= "first")

rf.roc <- wto.rf.aug %>% 
    roc_curve(truth = Frame,
              .pred_Recip, .pred_Redist,.pred_Unknown,
              event_level= "first")

svm.roc <- wto.svm.aug %>% 
    roc_curve(truth = Frame,
              .pred_Recip, .pred_Redist,.pred_Unknown,
              event_level= "first")

glm.roc <- wto.glm.aug %>% 
    roc_curve(truth = Frame,
              .pred_Recip, .pred_Redist,.pred_Unknown,
              event_level= "first")

class(nb.roc) ## gg, ggplot

## Plots:
pdf(file="ROCplot-naivebayes.pdf")
autoplot(nb.roc) +
    labs(title = "Naive Bayes"
         )
dev.off()

pdf(file="ROCplot-svm.pdf")
autoplot(svm.roc) +
    labs(title = "SVM"
         )
dev.off()

pdf(file="ROCplot-randomforest.pdf")
autoplot(rf.roc) +
    labs(subtitle = "Random Forest"
         )
dev.off()

pdf(file="ROCplot-glmlasso.pdf")
autoplot(glm.roc) +
    labs(subtitle = "GLM + LASSO"
         )
dev.off()

##%%%%%%%%%%%%%%%%%%%%%%%%%%
## Speaker-delegations
## From hand-coded
##%%%%%%%%%%%%%%%%%%%%%%%%%%
## Predicting based on delegations in training set
## on testing set

ls()
summary(tagged.train)

dim(tagged.train)

table(tagged.train$Frame)

## Identify speakers in tagged Redistributor paragraphs

redists <- as.data.frame(table(tagged.train[which(
    tagged.train$Frame=="Redist"),"firstent"]))

colnames(redists) <- c("Deleg", "Freq.Redist")

## Speakers in Reciprocator paragaphs
recip <- as.data.frame(table(tagged.train[which(
    tagged.train$Frame=="Recip"),"firstent"]))

colnames(recip) <- c("Deleg", "Freq.Recip")

unknown <- as.data.frame(table(tagged.train[which(
    tagged.train$Frame=="Unknown"),"firstent"]))

colnames(unknown) <- c("Deleg", "Freq.unknown")

## union in redistributor vs reciprocator:
intersect(redists$Deleg, recip$Deleg) ## Overlap:
## China, EU, US

## Merge together; "Pred probability" as % in dominant category

delegations.twoway <- merge(recip,
                     redists,
                     by="Deleg",
                     all=TRUE)

delegations.threeway <- merge(delegations.twoway,
                     unknown,
                     by="Deleg",
                     all=TRUE)

delegations.twoway[is.na(delegations.twoway)] <- 0

## Predict in test set based on top speaker in test set:
## Design "probabilities": Given that a paragraph is from a
## speaker in the reciprocator or reidstributor pool, what is the likelihood that the paragraph was a redist paragraph?
## (note, I'm not taking into account non-tagged paragraphs)
## so we know that it will be an over-estimate

allparas <- delegations.twoway$Freq.Recip +
    delegations.twoway$Freq.Redist

delegations.twoway$.pred_Redist <- round(
    delegations.twoway$Freq.Redist/allparas, 3)

delegations.twoway$.pred_Recip <- round(
    delegations.twoway$Freq.Recip/allparas, 3)

## Predict Class
delegations.twoway$.pred_class <- "Unknown"

delegations.twoway[which(delegations.twoway$.pred_Recip >
                         delegations.twoway$.pred_Redist),
                   ".pred_class"] <- "Recip"

delegations.twoway[which(delegations.twoway$.pred_Recip <
                         delegations.twoway$.pred_Redist),
                   ".pred_class"] <- "Redist"

### Merge into the test set:
wto.key.aug <- tagged.test

head(wto.key.aug)

cols <- c("Deleg", ".pred_Redist",
          ".pred_Recip", ".pred_class")

tst <- merge(wto.key.aug,
             delegations.twoway[,cols],
             by.x="firstent",
             by.y="Deleg",
             all.x=TRUE)

##also the test set, for later:
tst2 <- merge(tagged.train,
             delegations.twoway[,cols],
             by.x="firstent",
             by.y="Deleg",
             all.x=TRUE)

tst[is.na(tst$.pred_class), ".pred_class"] <- "Unknown"
tst2[is.na(tst2$.pred_class), ".pred_class"] <- "Unknown"

## Expected probabily of "unknown" = 100% if not in the
## recip/redist delegate groups. B/c this is the residual

tst[is.na(tst$.pred_class), ".pred_class"] <- "Unknown"

tst$.pred_Unknown <-0
tst[which(tst$.pred_class=="Unknown"),
    ".pred_Unknown"] <- 1

tst[is.na(tst$.pred_Redist), ".pred_Redist"] <- 0
tst[is.na(tst$.pred_Recip), ".pred_Recip"] <- 0


tst[,c("firstent", "Frame", ".pred_class",
       ".pred_Redist", ".pred_Recip",
       ".pred_Unknown")]

## The training set:

## Create predicted unknown:
tst2[is.na(tst2$.pred_class), ".pred_class"] <- "Unknown"
tst2$.pred_Unknown <-0
tst2[which(tst2$.pred_class=="Unknown"),
    ".pred_Unknown"] <- 1

tst2[is.na(tst2$.pred_Redist), ".pred_Redist"] <- 0
tst2[is.na(tst2$.pred_Recip), ".pred_Recip"] <- 0

## write entire hand-tagged + predicted:
cols2 <- c("PID",
           "Frame",
           ".pred_class",
           ".pred_Redist",
           ".pred_Recip",
           ".pred_Unknown")

tst.out <- rbind(tst[,cols2],
                 tst2[, cols2])

write.csv(tst.out, "wto-hand-class.csv")

### ROC Curve

hand.roc <- tst %>% 
    roc_curve(truth = Frame,
              .pred_Recip, .pred_Redist,.pred_Unknown,
              event_level= "first")

pdf(file="ROCPlot-delegatelists.pdf")
autoplot(hand.roc) +
    labs(subtitle = "Delegate Prevalence"
         )
dev.off()

##%%%%%%%%%%%%%%%%%%%%
## Compare all ROC
##%%%%%%%%%%%%%%%%%%%

ls()

roc.all.mods <- rbind(
    cbind(svm.roc, mod="SVM"),
    cbind(nb.roc, mod="NB"),
    cbind(glm.roc, mod="GLM"),
    cbind(rf.roc, mod="RF"),
    cbind(hand.roc, mod="Deleg"))


class(roc.all.mods)
head(roc.all.mods)

gg <- ggplot(roc.all.mods,
             aes(x = 1 - specificity,
                 y = sensitivity,
                 color=mod)) +
    geom_path() +
    geom_abline(lty = 3) +
    coord_equal() +
    theme_bw()+
    facet_wrap(~.level)


gg
ggsave(gg, file="classificationAllMods.pdf")

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Scale prediction from RF Model:
## Whole data:

## predict:

colnames(meta)[which(colnames(meta)=="pid")] <- "PID"

tagged.pids <- tagged$PID

length(tagged.pids) ##239

untagged <- meta[!(meta$PID %in% tagged.pids),]

dim(untagged) ##8614 x 16
                
rf.pred.all <- predict(rf.fit, untagged)
wto.meta.aug <- augment(rf.fit, untagged)

write_csv(wto.meta.aug,
          "wta-rf-cat.csv")

