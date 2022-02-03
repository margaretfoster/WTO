## Generate figures that show topic proportions by
## faction and framing categories

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('stm', 'ggplot2',
           'dplyr','tidyr',
           'stminsights')

loadPkg(packs)

ls()

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
## Data 1: K=10 model on Program subset
#############################
load(paste0(dataPathDesktop,
            "ProcessSubSetstmYearStemmedInc.Rdata"))

## Verify the specification:

mod.process.themeinc$settings$call
inc.model ## ~s(year)*income

## Refresh factor levels:

unique(out$meta$income_level_iso3c) ##"HIC" "LMC"
##"AGG" "UMC" "LIC" 

## get estimated effects:


ggdat <- stminsights::get_effects(
    estimates=prep.process.themeinc,
    variable="year",
    type="continuous",
    moderator="income_level_iso3c",
    modval=c("HIC")) %>%
    bind_rows(
        get_effects(
            estimates=prep.process.themeinc,
            variable="year",
            type="continuous",
            moderator="income_level_iso3c",
            modval=c("LMC"))) %>%
        bind_rows(
            get_effects(
            estimates=prep.process.themeinc,
            variable="year",
                type="continuous",
                moderator="income_level_iso3c",
                modval=c("AGG"))) %>%
    bind_rows(
        get_effects(
            estimates=prep.process.themeinc,
            variable="year",
            type="continuous",
            moderator="income_level_iso3c",
            modval=c("UMC"))) %>%
    bind_rows(
        get_effects(
            estimates=prep.process.themeinc,
            variable="year",
            type="continuous",
            moderator="income_level_iso3c",
            modval=c("LIC")))

dim(ggdat)## 1300 x 6

head(ggdat) 

## Name the topics:

ggdat$topicnames <- "tbd"
ggdat[which(ggdat$topic==1), "topicnames"] <-
    "Notif."
ggdat[which(ggdat$topic==2), "topicnames"] <-
    "GSP"
ggdat[which(ggdat$topic==3), "topicnames"] <-
    "Obser."
ggdat[which(ggdat$topic==4), "topicnames"] <-
    "Gridlock"
ggdat[which(ggdat$topic==5), "topicnames"] <-
    "GG"
ggdat[which(ggdat$topic==6), "topicnames"] <-
    "Delegs."
ggdat[which(ggdat$topic==7), "topicnames"] <-
    "GCC"
ggdat[which(ggdat$topic==8), "topicnames"] <-
    "Policies"
ggdat[which(ggdat$topic==9), "topicnames"] <-
    "Reports"
ggdat[which(ggdat$topic==10), "topicnames"] <-
    "Agenda"

ls()

colnames(ggdat)

## Plots:
## Filter out LIC:
ilevels <- c("HIC", "LMC", "AGG", "UMC")

ggdat %>%
    ##filter(topic == 1) %>%
    ##filter(moderator %in% ilevels) %>%
   ## filter(moderator ==  "LIC") %>% ## LIC blows up for all topics
    mutate(moderator = as.factor(moderator)) %>%
    ggplot(aes(x = value, y = proportion,
               color = moderator,
               group = moderator,
               fill = moderator)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = 0.1)  +
    theme_light() +
    labs(x = 'Year', y = 'Topic Proportion',
         color = 'Treatment',
         group = 'Treatment', fill = 'Treatment')+
    facet_grid(moderator ~ topicnames,
               scales="free")+
       theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, size=rel(.75)))

ggsave(file="ProcessIncMinusLIC.png")
