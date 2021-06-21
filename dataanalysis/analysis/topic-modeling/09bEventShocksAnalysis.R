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
           'tidyr', 'quanteda')

loadPkg(packs)


#########################
## Declare Data Paths
#########################

if(Sys.info()['user']=="Ergane"){ ## if on my own machine look in Dropbox
    dataPathDesktop <- "~/Dropbox/WTO-Data/rdatas/"
    print(paste0("The datapath is: ", dataPathDesktop))
}else{ ## else look in ~/WTO/
    dataPathDesktop <- "../../"
    print(paste0("The datapath is: ", dataPathDesktop))
}

#############################
## Load Processed Data & Models
#############################

load(paste0(dataPathDesktop,"processedTextforSTM.Rdata"))
##load(paste0(dataPathDesktop, "TrumpEffect.Rdata"))
##load(paste0(dataPathDesktop, "ChinaJoinsShockEffect.Rdata"))
load(paste0(dataPathDesktop, "FCShockEffect.Rdata"))

     
############################
##### Analysis
############################

topicNames <- c("The Chair Thanked",
                "Observers", "Multilateral Trade",
                "PTAs", "Vulnerable States",
                "GCC",  "Ecommerce", "Support", "Metadata",
                "Budget", "Scheduling", "New Reps",
                "Tech. Presentations", "Tariffs/Market Access",
                "Trade Agreements", "Stalled Progress",
                "CTD Mandate", "Technical Assistance",
                "Diff. Treatment", "Capacity Building",
                "Training programmes", "Annual Audit",
                "EU Frustration",
                "Doha Declaration")

ls()

## First prevalence before/after the FC
## Income levels: "UMC"   "LMC"   "NOTST" "AGG"   "HIC"   "LIC"

dev.off()

par(mfrow=c(3,2))

plot.estimateEffect(prep.FC,
                    covariate="postFC",
                    method="difference",
                    verbose.labels="TRUE",
                    cov.value1=1,
                    cov.value2=0,
                    moderator="iso3c.il",
                    moderator.value="HIC",
                    labeltype="custom",
                    custom.labels=topicNames,
                    main="Topic Prevalance Before/After Financial Crisis, HIC Delegations",
                    xlab="Topic Proportions After vs Before FC",
                    sub=)

plot.estimateEffect(prep.FC,
                    covariate="postFC",
                    method="difference",
                    cov.value1=1,
                    cov.value2=0,
                    moderator="iso3c.il",
                    verbose.labels="FALSE",
                    custom.labels=topicNames,
                    xlab="Topic Proportions After vs Before FC",
                    moderator.value="AGG",
                    main="AGG Delegations")

plot.estimateEffect(prep.FC,
                    covariate="postFC",
                    method="difference",
                    cov.value1=1,
                    cov.value2=0,
                    verbose.labels="FALSE",
                    custom.labels=topicNames,
                    moderator="iso3c.il",
                    moderator.value="UMC",
                    xlab="Topic Proportions After vs Before FC",
                    main="UMC Delegations")
plot.estimateEffect(prep.FC,
                    covariate="postFC",
                    method="difference",
                    cov.value1=1,
                    cov.value2=0,
                    verbose.labels="FALSE",
                    custom.labels=topicNames,
                    moderator="iso3c.il",
                    moderator.value="LMC",
                    xlab="Topic Proportions After vs Before FC",
                    main="LMC Delegations")
plot.estimateEffect(prep.FC,
                    covariate="postFC",
                    method="difference",
                    cov.value1=1,
                    cov.value2=0,
                    moderator="iso3c.il",
                    verbose.labels="FALSE",
                    custom.labels=topicNames,
                    xlab="Topic Proportions After vs Before FC",
                    moderator.value="LIC",
                    main="LIC Delegations")

dev.off()

## Then work through perspectives before and after
## Financial Crisis.
