 
### Preliminary analysis of trade and development
## Using speaker-level metadata about country wealth
## from World Bank data

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
            { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) ) }}


packs <- c('tm', 'stm', 
           'tidyr', 'quanteda')

loadPkg(c(packs))

######## Replacing yvarlist storage issue with
######## package examples

head(gadarian)

temp<-textProcessor(documents=gadarian$open.ended.response,
                    metadata=gadarian)

out <- prepDocuments(temp$documents,
                     temp$vocab,
                     temp$meta)

colnames(temp$meta)
table(temp$meta$treatment) ## 0 [170]; 1 [171]

set.seed(02138)
mod.out <- stm(out$documents,
               out$vocab,
               K=3,
               prevalence=~treatment + s(pid_rep),
               data=out$meta)

summary(gadarianFit)
plot(gadarianFit,
     type="perspectives",
     topics=c(3),
     covarlevels=c("0", "1"))

summary(mod.out)

attributes(mod.out)
attributes(mod.out$settings)
attributes(mod.out$settings$covariates)
mod.out$settings$covariates$yvarlevels


########################
## Declare Data Paths
#########################



if(Sys.info()['user']=="Ergane"){ ## my desktop
    dataPathDesktop <- "~/Dropbox/WTO-Data/rdatas/"
    print(paste0("The datapath is: ", dataPathDesktop))
}else{
    if(Sys.info()['user']=="Promachos"){ ##my laptop
        dataPathDesktop <- "~/Dropbox/WTO-Data/rdatas/"
        print(paste0("The datapath is: ", dataPathDesktop))
    }else{ ## else look in ~/WTO/
        dataPathDesktop <- ""
        print(paste0("The datapath is: ", dataPathDesktop))
    }}

## This has the WTO paragraph-level data,
## post-processing cleanup
## and wealth info
load(paste0(dataPathDesktop,"K24-25Model.Rdata"))
load(paste0(dataPathDesktop,"K85model.Rdata"))
load(paste0(dataPathDesktop,"K100model.Rdata"))


#################
### Try to pull apart the
## "perspectives" plotting
##4/9/21
#####################

library(stm)


### This gives me an error that I have to set the xlim
stm::plot.STM(x=mod.out.25,
     type="perspectives",
     topics=23,
     covarlevels=c("HIC","LIC"),
     )

### Try to rebuild the plot by pulling out the data

attributes(mod.out.25) ## settings

attributes(mod.out.25$settings) ## covariates

mod.out.25$settings$call ## model
## model <- ~s(numdate) * as.factor(income_level_iso3c)

class(mod.out.25$settings$covariates) ## list
length(mod.out.25$settings$covariates) ## 4
names(mod.out.25$settings$covariates) ## X, betaindex, yvarlevels, formula

mod.out.25$settings$covariates$formula ## that's the variables

mod.out.25$settings$covariates$yvarlevels

names(mod.out.25$settings$covariates$X)

mod.out.25$settings$covariates$yvarlevels

ls()

mod.100$settings$covariates$yvarlevels

mod.100$settings$covariates$formula

### Still not having the covariate values:
## trying a toy run:

docs<-out$documents
vocab<-out$vocab
meta <-out$meta
set.seed(02138)

mod.out <- stm(docs,
               vocab,
               K=3,
               prevalence=~s(numdate) +
               income_level_iso3c,
               data=meta)

attributes(mod.out)
attributes(mod.out$settings)
attributes(mod.out$settings$covariates)
length(mod.out$settings.covariates$yvarlevels)
mod.out$settings.covariates$yvarlevels ## still nothing

plotModels(mod.out)
ls()

class(out)
attributes(out)



