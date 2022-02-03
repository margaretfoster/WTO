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
            "ProcessSubSetstmYearStemmedFac.Rdata"))

ls()

## Verify the specification:
mod.process.themefaction$settings$call
faction.model ## ~s(year)*faction

## Refresh factor levels:
unique(out$meta$faction) ## US-EU-Can; China-Egypt-India; Other

## get estimated effects:

ggdat <- stminsights::get_effects(
    estimates=prep.process.themefaction,
    variable="year",
    type="continuous",
    moderator="faction",
    modval=c("US-EU-Can")) %>%
    bind_rows(
        get_effects(
            estimates=prep.process.themefaction,
            variable="year",
            type="continuous",
            moderator="facation",
            modval=c("China-Egypt-India"))) %>%
        bind_rows(
            get_effects(
            estimates=prep.process.themefaction,
            variable="year",
                type="continuous",
                moderator="faction",
                modval=c("Other")))

dim(ggdat)## 3000 x 6

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
ilevels <- c("US-EU-Can",
             "China-Egypt-India",
             "Other")

ggdat %>%
    ##filter(topic == 1) %>%
    filter(moderator %in% ilevels) %>%
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
    theme(axis.text.x = element_text(angle = 45))

ggsave(file="ProcessFactionOverview.png")
