### Preliminary analysis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('dplyr', 'tidyr',
           'ggplot2')


loadPkg(packs)


#########################
## Declare Data Paths
#########################

if(Sys.info()['user']=="Ergane" |
   Sys.info()['user']== 'Promachos'){ ## if on my own machine look in Dropbox
    dataPathDesktop <- "~/Dropbox/WTO-Data/rdatas/"
    print(paste0("The datapath is: ", dataPathDesktop))
}else{ ## else look in ~/WTO/
    dataPathDesktop <- "../../"
    print(paste0("The datapath is: ", dataPathDesktop))
}

#############################
## Load Processed Data
## (Working with out$meta)
#############################

load(paste0(dataPathDesktop,"processedTextforSTM.Rdata"))
     
############################
##### Analysis: 
############################

dim(out$meta) ## 8856 x 16

## data in speaker-turn model
## need: speaker-meeting dataframe

delegatesums <- out$meta %>% 
    group_by(meetingno, country) %>%
    summarise(meetingtotal = n())

dim(delegatesums) ## 2361

length(unique(delegatesums$country)) ## 124

## Merge in state-level income variables:

colnames(out$meta)

metacols <- c('iso3c',"country",
              'income_level_iso3c', 'region')

metadat <- unique(out$meta[,metacols])

dim(metadat) ## 125 x 4

delegatesums <- merge(x= delegatesums,
                      y= metadat,
                      by= "country")

dim(delegatesums) ## 2471 x 6

table(delegatesums$region)
table(delegatesums$income_level_iso3c)
## admin= 113
## AGG = 108 [this is the EU]
## HIC = 742
## LIC = 106
## LMC = 745
## notstat = 113
## UMC = 546
## Interesting is how much the table of activity
##shows that the
## CTD is an HIC and LMC space

## consolidate the EU into HIC

delegatesums$income <- delegatesums$income_level_iso3c

delegatesums[which(delegatesums$income=="AGG"),
             "income"] <- "HIC" ## Adding EU to HIC

## Aggregates is also all the EU, make that Europe
delegatesums[which(delegatesums$region=="Aggregates"), "region"] <- "Europe & Central Asia"

table(delegatesums$income)
table(delegatesums$region)

## drop admin and not-state:

dim(delegatesums)

##### Plot activity:

colnames(delegatesums)

regionswanted <- c("East Asia & Pacific",
                   "Europe & Central Asia",
                   "Latin America & Caribbean",
                   "Middle East & North Africa",
                   "North America",
                   "South Asia",
                   "Sub-Saharan Africa")

gg <- ggplot(data=delegatesums[which(
                 delegatesums$region %in% regionswanted), ],
             aes(x=meetingno,
                 y=meetingtotal,
                 group= country)) +
    geom_line(aes(color=region))+
    theme_bw()+
    facet_wrap(~ income) +
    labs(title = "Speaker Activities by Region",
       subtitle = "WTD CTD",
         y = "Speaker-turns per meeting",
         x = "Meeting Number") 
gg

delegatesums[,1:3] %>%
    arrange(country, meetingno)


summary(delegatesums$meetingno) ## 1-113


print(delegatesums[which(delegatesums$meetingno==113),1:3]) ## Bangladesh, Botsawa, US, Nonstate Repo [which is the chair speaking also about Afghanistan]



