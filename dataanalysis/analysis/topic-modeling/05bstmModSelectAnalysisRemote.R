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

#####################
## Load model Selection Object
#####################

###################
#### K270
###################

load(paste0(dataPathDesktop, "tradDevModSelect_20.RData"))

plotModels(mod.select.20)


