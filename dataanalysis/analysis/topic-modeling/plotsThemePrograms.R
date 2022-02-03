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
load("ProgramsSubSetstmStemmed.Rdata")
## For summary statistics;

ls()
## Point estimate plots per topic by meeting
## with different proportions by redistributor/reciprocator
## classification

mod.out10.themecat$settings$call

category.model ## model: content ~cat

levels(out$meta$cat)

ggdat <- stminsights::get_effects(
    estimates=prep.out10.themecat,
    variable="meetingno",
    type="pointestimate",
    moderator="cat",
    modval=c("Reciprocators")) %>%
    bind_rows(get_effects(
        estimates=prep.out10.themecat,
        variable="meetingno",
        type="pointestimate",
        moderator="cat",
        modval=c("Redistributors"))
              )

## name the topics:
ggdat$topicnames <- "tbd"
ggdat[which(ggdat$topic==1), "topicnames"] <-
    "Differential Treatment"
ggdat[which(ggdat$topic==2), "topicnames"] <-
    "OPEC & Observers"
ggdat[which(ggdat$topic==3), "topicnames"] <-
    "Commodities — Producers"
ggdat[which(ggdat$topic==4), "topicnames"] <-
    "Doha Para 51"
ggdat[which(ggdat$topic==5), "topicnames"] <-
    "Tech - EC Prefs"
ggdat[which(ggdat$topic==6), "topicnames"] <-
    "DFQF - LDC Prefs "
ggdat[which(ggdat$topic==7), "topicnames"] <-
    "Tech - Delivery"
ggdat[which(ggdat$topic==8), "topicnames"] <-
    "Training Programs"
ggdat[which(ggdat$topic==9), "topicnames"] <-
    "Vulnerable Economies"
ggdat[which(ggdat$topic==10), "topicnames"] <-
    "DFQF - Reviews"

colnames(ggdat)
colnames(out$meta)

## merge in year:

tmp <- as.data.frame(table(out$meta$year))
tmp

tmp2 <- as.data.frame(table(ggdat$value))
tmp2

cols.to.keep <- c("meetingno",
                  "year")

meetingmeta <-unique(out$meta[,cols.to.keep])

dim(meetingmeta) ##108; but I am looking for 113
## (Need to debug what happened to meetings in 2010)


levels(ggdat$value)

sort(unique(out$meta$meetingno))[70:80]
sort(unique(ggdat$value))[70:80]


ggdat2 <- merge(x=ggdat,
               y=meetingmeta,
               by.x="value",
               by.y="meetingno",
               all.x=TRUE
               )

table(ggdat2$year) ## nothing in 2009 or 2010?
table(out$meta$year)

## 2009: 73, 75, 76, 77
## 2010: 78, 79, 80 
unique(out$meta[which(out$meta$year==2009),]$meetingno)
unique(out$meta[which(out$meta$year==2010),]$meetingno)

dim(ggdat)

dev.off()
