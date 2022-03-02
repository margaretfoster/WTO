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


packs <- c('stm')
loadPkg(packs)

load("PerspectivesThemeProgramsYearStemmedFacRepl.Rdata")


### Make Tables For Topics Across Time
## And for estimate effects

## Topic Names via STM Viewer
topicNames.fac <- c("Market Access",
                    "Commodities",
                    "E-Commerce",
                    "TA Needed",
                    "TA Review",
                    "Support",
                    "Regional Courses",
                    "Special Provisions",
                    "Review Programs",
                    "Small Economies")

png(file="programsFactionTopics.png")
par(mfrow=c(4,3))
for(i in 1:10){
    plot.estimateEffect(x=prep.programs.themefac,
                        model=mod.programs.themefac,
                        covariate="year",
                        main=topicNames.fac[i],
                        topics=c(i), 
                        method="continuous",
                        xlab="Year",
                        ylab="E. Topic Prop.",
                        labeltype="custom",
                        printlegend=FALSE,
                        n=5,
                        linecol=c("darkgreen")
                        )}
dev.off()


###

rm(list=ls())

load("PerspectivesThemeProgramsYearStemmedIncRepl.Rdata")

ls()

topicNames.inc <- c("Small Economies",
                    "Market Access",
                    "Proposals",
                    "TA Requests",
                    "Training Requests",
                    "Support",
                    "Regional",
                    "Scope",
                    "E-commerce",
                    "Liberalization")



png(file="programsTopicsIncome.png")
par(mfrow=c(4,3))
for(i in 1:10){
    plot.estimateEffect(x=prep.programs.themeinc,
                        model=mod.programs.themeinc,
                        covariate="year",
                        main=topicNames.inc[i],
                        topics=c(i), 
                        method="continuous",
                        xlab="Year",
                        ylab="E. Topic Prop.",
                        labeltype="custom",
                        printlegend=FALSE,
                        n=5,
                        linecol=c("darkgreen")
                        )}
dev.off()




