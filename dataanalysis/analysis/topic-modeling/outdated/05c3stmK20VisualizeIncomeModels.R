 

## This script to make 5 2x4 visualizationsn
## of the topic prevalences over time
## according to speaker income type

######################
## Declare Data Paths
######################

if(Sys.info()['user']=="Ergane"){## desktop                                                                                                                          
    dataPathDesktop <- "~/Dropbox/WTO/rdatas/"
    print(paste0("On desktop, data path is ", dataPathDesktop))
}else{ ## any other machine                                                                                                                                          
    dataPathDesktop <- "../../"
    print(paste0("On remote, data path is ", dataPathDesktop))
}

library(stm)
library(stringr)
library(lubridate)
library(ggplot2)
library(devtools)
library(tidystm)
###################################
## Load model
###################################
                
####################################
#### K20
####################################

load(paste0(dataPathDesktop, "tradDevPara_20InteractNetActSend.RData"))

ls()

## Models:

## prep.20.rec estimated effect for net recievers
## prep.20.send is estimated effct for net senders

## Compare the two models:
pdf(file="NetworkActiveModelsComparision.pdf")
par(mfrow=c(1,2))
plot(mod.out.20.rec, main="Summary Active Recievers Model")
plot(mod.out.20.send, main="Summary Active Senders Model")
dev.off()


## mod.out.20 is the topic model results
## prep.20 is the estimated effects

## function is:
## s(numdate)*as.factor(delttype)


######################
## Experimenting with tidystm + ggplot

## extract for modularity (and ease of typing):mvar <- "delttype" ## moderator variable

ls()
n
names(mod.out.20.send)

names(mod.out.20.send$settings)

mod.out.20.send$settings$call
mod.out.20.rec$settings$call

### rename levels to make graphs more pretty:
mvarSend <- "NetActSend"
mvarRec <- "NetActInRef"

mvarOrder <- c("None/Low", "Top20P","Top10P", "Top05P")

mvalsSend <- levels(factor(out$meta[,mvarSend],
                           levels=mvarOrder)) #levels of mod. variable
mvalsSend

mvalsRec <- levels(factor(out$meta[,mvarRec],
                             levels=mvarOrder)) #levels of mod. variable
mvalsRec


mcols <- c("darkgray", ## None/Low
           "navyblue", ## Top 20%
           "maroon",## Top 10%
           "darkgreen") ##Top 5%

ls()
### Extract Effect, sender activity model
effect.Send <- lapply(mvalsSend, function(l) { ## creates a 6-entry list
    extract.estimateEffect(x=prep.20.send, ## pulls out the estimated effect
                           covariate = "numdate",
                           model = mod.out.20.send,
                           method = "continuous",
                           labeltype = "frex",
                           n= 4 ,
                           moderator = mvarSend,
                           moderator.value=l)
}) ## note: 1-2 minutes to run


effect.Send <- do.call("rbind", effect.Send)

dim(effect.Send) ##
colnames(effect.Send) 

#### Extract Effect, references recieved model

effect.Rec <- lapply(mvalsRec, function(l) { ## creates a 6-entry list
    extract.estimateEffect(x=prep.20.rec, ## pulls out the estimated effect
                           covariate = "numdate",
                           model = mod.out.20.rec,
                           method = "continuous",
                           labeltype = "frex",
                           n= 4 ,
                           moderator = mvarRec,
                           moderator.value=l)
}) ## note: 1-2 minutes to run

effect.Rec <- do.call("rbind", effect.Rec)

dim(effect.Rec) ##
colnames(effect.Rec)
  
##################################
##################################
## Add column for date:
## R's date conversion started at 1970-01-01
## so: need to round the covariate.value, and convert to
## a date:

mds <- unique(out$meta$date)

###
effect.Send$covdate <- as.Date(effect.Send$covariate.value,
                                origin="1970-01-01")

effect.Rec$covdate <- as.Date(effect.Rec$covariate.value,
                               origin="1970-01-01")

years <- seq(from=1995, to=2019, by=1)

## effect$topic = 1:20 with 600 entries each
## effect$label = the top four words for each topic

#############################################
## Functions for plotting
#############################################


## truncate the lower bounds of CI, and point estimates at 0
## Justifable because the negative CI and estimates are an artifact
  
colnames(effect.Rec)
summary(effect.Rec$ci.lower)
summary(effect.Send$ci.lower)
summary(effect.Rec$estimate)
summary(effect.Rec$estimate)

## keep the estimates that are in the scope we want:

summary(effect.Rec$ci.lower)
summary(effect.Send$ci.lower)
summary(effect.Rec$estimate)
summary(effect.Rec$estimate)
summary(effect.Rec$ci.upper)
summary(effect.Send$ci.upper)

############################


plot4s <- function(datasubset){
    
    ggplot(datasubset, aes(x = covdate, y = estimate,
                       ymin = ci.lower, ymax = ci.upper,
                       group = moderator.value,
                       fill = factor(moderator.value))) +
        facet_wrap(~ label, nrow = 3) +
         geom_line() + ## the point estimates
        scale_fill_manual(values=mcols)+
        geom_ribbon(alpha = .4) +
        scale_x_date(date_breaks='1 year',
                     date_labels= "%Y")+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    geom_hline(yintercept=0, ## line at 0
               linetype="dashed",
               color="red")+
        ylim(0,1) + #zoom in
        labs(x = "Year",
              y = "Expected Topic Proportion, [0, 1]",
             fill = "Level of Network Activiy") +
        theme(legend.position = "bottom")
}


plot4r <- function(datasubset){
    
    ggplot(datasubset, aes(x = covdate, y = estimate,
                       ymin = ci.lower, ymax = ci.upper,
                       group = moderator.value,
                       fill = factor(moderator.value))) +
        facet_wrap(~ label, nrow = 3) +
         geom_line() + ## the point estimates
        ## scale_fill_manual()
        geom_ribbon(alpha = .4) +
        scale_x_date(date_breaks='1 year',
                     date_labels= "%Y")+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    geom_hline(yintercept=0, ## line at 0
               linetype="dashed",
               color="red")+
        ylim(0,1) + #zoom in
        labs(x = "Year",
              y = "Expected Topic Proportion, [0, 1]",
             fill = "Level of Network Activiy") +
        theme(legend.position = "bottom")
}


#### Subset topics into groupings of four

### just truncating was a terrible idea

Er2 <- effect.Rec
Es2 <- effect.Send



Er2[which(Er2$ci.lower < 0), "ci.lower"] <- 0
Er2[which(Er2$ci.upper > 1), "ci.upper"] <- 1
Er2[which(Er2$estimate > 1), "estimate"] <- 1

Es2[which(Es2$ci.lower < 0), "ci.lower"] <- 0
Es2[which(Es2$ci.upper > 1), "ci.upper"] <- 1
Es2[which(Es2$estimate > 1),"estimate"] <- 1

 
  
sub1r <- Er2[which(Er2$topic %in% c(1:4)),]
sub2r <- Er2[which(Er2$topic %in% c(5:8)),]
sub3r <- Er2[which(Er2$topic %in% c(9:12)),]
sub4r <- Er2[which(Er2$topic %in% c(13:16)),]
sub5r <- Er2[which(Er2$topic %in% c(17:20)),]

sub1s <- Es2[which(Es2$topic %in% c(1:4)),]
sub2s <- Es2[which(Es2$topic %in% c(5:8)),]
sub3s <- Es2[which(Es2$topic %in% c(9:12)),]
sub4s <- Es2[which(Es2$topic %in% c(13:16)),]
sub5s <- Es2[which(Es2$topic %in% c(17:20)),]

##### Most active recievers plots
pdf(file="PlotTopics1to4Rec.pdf")
plot4r(sub1r)
dev.off()

pdf(file="PlotTopics5to8Rec.pdf")
plot4r(sub2r)
dev.off()

pdf(file="PlotTopics9to12Rec.pdf")
plot4r(sub3r)
dev.off()

pdf(file="PlotTopics13to16Rec.pdf")
plot4r(sub4r)
dev.off()

pdf(file="PlotTopics17to20Rec.pdf")
plot4r(sub5r)
dev.off()

## most active senders plots

pdf(file="PlotTopics1to4Send.pdf")
plot4s(sub1s)
dev.off()

pdf(file="PlotTopics5to8Send.pdf")
plot4s(sub2s)
dev.off()

pdf(file="PlotTopics9to12Send.pdf")
plot4s(sub3s)
dev.off()

pdf(file="PlotTopics13to16Send.pdf")
plot4s(sub4s)
dev.off()

pdf(file="PlotTopics17to20Send.pdf")
plot4s(sub5s)
dev.off()


#########################
## 1x2 topics
#######################

plot2 <- function(datasubset){
    
    ggplot(datasubset, aes(x = covdate, y = estimate,
                       ymin = ci.lower, ymax = ci.upper,
                       group = moderator.value,
                       fill = factor(moderator.value))) +
        facet_wrap(~ label, nrow = 2) +
         geom_line() + ## the point estimates
        scale_fill_manual(values=mcols)+
        geom_ribbon(alpha = .2) +
        scale_x_date(date_breaks='1 year',
                     date_labels= "%Y")+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    geom_hline(yintercept=0, ## line at 0
               linetype="dashed",
               color="red")+
        ylim(-.45,.7) + #zoom in
        labs(x = "Year",
              y = "Expected Topic Proportion",
             fill = "Income") +
        theme(legend.position = "bottom")
}

## Differences in preferred focus LDCs vs all others:
sub6 <- effect[which(effect$topic %in% c(13, 16)),]


pdf(file="PlotTopicsEndCompare.pdf")
plot4(sub6)
dev.off()


## Differences in preferred focus LDCs vs all others:
sub7 <- effect[which(effect$topic %in% c(8, 17)),]


pdf(file="PlotTopicsLDCEmphasis1.pdf")
plot4(sub7)
dev.off()

## Differences in preferred focus LDCs vs all others:
sub8 <- effect[which(effect$topic %in% c(11, 13)),]


pdf(file="PlotTopicsLDCEmphasis2.pdf")
plot4(sub8)
dev.off()
