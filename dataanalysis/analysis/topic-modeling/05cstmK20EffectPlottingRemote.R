##library(stminsights)


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

###################################
## Load model topic model
###################################

###################
#### K270
###################

load(paste0(dataPathDesktop, "tradDevPara_20Interact.RData"))

## mod.out.20 is the topic model results
## prep.20 is the estimated effects
## function is:
## s(numdate)*as.factor(income_level_iso3c)

plot(mod.out.20)

###################################
### Plotting over time
###################################

### Substantive interest:
## What hapens wot the small economies topic?

plot(prep.20,
     topics=c(5),
     covariate="numdate",
     moderator="income_level_iso3c",
     moderator.value="LIC",
     method="continuous"
     )
