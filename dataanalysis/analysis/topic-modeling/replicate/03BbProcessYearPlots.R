### Preliminary analysAis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            {install.packages(
                lib, repos='http://cran.rstudio.com/') }
        suppressMessages(library(lib,
                                 character.only=TRUE))}}

packs <- ("stm")
loadPkg(packs)

#############################
## Load Processed Data
#############################
load("PerspectivesThemeProcessYearStemmedFacRepl.Rdata")


### Make Tables For Topics Across Time
## And for estimate effects
topicNames.fac <- c("GSP Scheme",
                    "Mandate Work",
                    "Agenda Items",
                    "Comments",
                    "RTA Notif.",
                    "LAIA Notif.",
                    "Feasibility",
                    "Oppose EC",
                    "Secretariat",
                    "Observers")


png(file="processFactionTopics.png")
par(mfrow=c(4,3))
for(i in 1:10){
    plot.estimateEffect(x=prep.process.themefac,
                        model=mod.process.themefac,
                        covariate="year",
                        main=topicNames.fac[i],
                        topics=c(i), 
                        method="continuous",
                        xlab="Year",
                        ylab="E. Topic Prop.",
                        labeltype="custom",
                        printlegend=FALSE,
                        n=5,
                        linecol=c("darkblue")
                        )}
dev.off()



##%%%%%%%%%%%%%%%
## For appendix: Income Breakout
 load("PerspectivesThemeProcessYearStemmedIncRepl.Rdata")

### Make Tables For Topics Across Time
## And for estimate effects


topicNames.inc <- c(
    "GCC",
    "Proposals",
    "RTA Notifications",
    "Comments",
    "Agenda Items",
    "LAIA Notifications",
    "Reports",
    "DQDF for LDCs",
    "Secretariat",
    "Observer Status")

png(file="processTopicsInc.png")
par(mfrow=c(4,3))
for(i in 1:10){
    plot.estimateEffect(x=prep.process.themeinc,
                        model=mod.process.themeinc,
                        covariate="year",
                        main=topicNames.inc[i],
                        topics=c(i), 
                        method="continuous",
                        xlab="Year",
                        ylab="E. Topic Prop.",
                        labeltype="custom",
                        printlegend=FALSE,
                        n=5,
                        linecol=c("darkblue")
                        )}
dev.off()




