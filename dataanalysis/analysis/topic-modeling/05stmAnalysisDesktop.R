##library(stminsights)

library(stminsights)

######################
## Declare Data Paths
######################

if(Sys.info()['user']=="Ergane"){## desktop                                                                                                                          
    dataPathDesktop <- "~/Dropbox/WTO-Data/rdatas/"
    print(paste0("On desktop, data path is ", dataPathDesktop))
}else{ ## any other machine                                                                                                                                          
    dataPathDesktop <- "../../"
    print(paste0("On remote, data path is ", dataPathDesktop))
}

library(stm)

#####
## Summarize Searchk for the full meeting range

load(paste0(dataPathDesktop, "sK10to100M1to113.Rdata"))

ls()

class(mod.tdsk)

plot(mod.tdsk)

#####################
## Visualize different models
#####################




