##library(stminsights)


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
## Summarize previous runs:
#####################

###################
#### K=70
###################

load(paste0(dataPathDesktop, "tradDevPara_70.Rdata"))

ls()

sink(file="SummaryModK70.txt")
summary(mod.out.70)
sink()

######################
### K= 20
#####################

load(paste0(dataPathDesktop, "tradDevPara_20.Rdata"))

ls()

sink(file="summaryModK20.txt")
summary(mod.out.20)
sink()

#######################
## K 20 with interaction
## on time and income
#######################

load(paste0(dataPathDesktop,
            "tradDevPara_20Interact.RData"))

ls()

sink(file="summaryModK20Interact.txt")
summary(mod.out.20)
sink()


#######################
## K 20 with interaction
## on time and network activity 
#######################

load(paste0(dataPathDesktop,
            "tradDevPara_20InteractDeltType.RData"))

ls()

sink(file="summaryModK20Interact.txt")
summary(mod.out.20)
sink()





