##library(stminsights)

## Declare Data Paths
if(Sys.info()['user']=="Ergane"){## desktop                                                                                                                          
    dataPathDesktop <- "~/Dropbox/WTO/rdatas/"
    print(paste0("On desktop, data path is ", dataPathDesktop))
}else{ ## any other machine                                                                                                                                          
    dataPathDesktop <- "~/Dropbox/WTO/"
    print(paste0("On remote, data path is ", dataPathDesktop))
}

library(stm)

####K-70
load(paste0(dataPathDesktop, "tradDevPara_70.Rdata"))

ls()

sink(file="SummaryModK70.txt")
summary(mod.out.70)
sink()

### K-20

load(paste0(dataPathDesktop, "tradDevPara_20.Rdata"))

ls()

sink(file="summaryModK20.txt")
summary(mod.out.20)
sink()
