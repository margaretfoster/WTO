##library(stminsights)

######################
## Declare Data Paths
######################

if(Sys.info()['user']=="Ergane"){## desktop                              
    dataPathDesktop <- "~/Dropbox/WTO-Data/rdatas/"
    print(paste0("On desktop, data path is ", dataPathDesktop))
}else{ ## any other machine                                              
    dataPathDesktop <- "../../../"
    print(paste0("On remote, data path is ", dataPathDesktop))
}

library(stm)

#####
## Summarize Searchk for the full meeting range

load(paste0(dataPathDesktop, "tradDevParaM1to113-Pandemic.RData"))

ls()

#####################
## Summarize output from models
#####################
## K21 model:  Look to see if the "vulnerable"
## economies rises at the end b/c of pandemic

## K85 model: look for rates of pandemic
## topic

######################
### K= 21
#####################

summary(mod.out.21)

## topic 5 is the vulnerable economies
## some increase, but hard to distinguish from
## noise at the end of the estimation timeline

plot(prep.21,
     topics=c(5),
     "numdate",
     method="continuous")


##Extract topics:

theta21Out <- as.data.frame(round(mod.out.21$theta, 4))

dim(theta21Out) ## 8553x 21

head(theta21Out)
## replace "V-" in the column names
colnames(theta21Out) <- gsub(pattern="V",
                             replace="M21.Topic",
                             x=as.character(colnames(theta21Out)))
theta21Out <- cbind(meta$pid, theta21Out)

head(theta21Out)

#######################
## K 85
#######################

#sink(file="modOut85Summary.txt")
summary(mod.out.85)
#sink()

attributes(mod.out.85)
attributes(mod.out.85$theta) ## 8853 x 85. So this is the estimated topic
## proportion for each paragraph

theta85Out <- as.data.frame(round(mod.out.85$theta, 4))
colnames(theta85Out) <- gsub(pattern="V",
                             replace="M85.Topic",
                             x=as.character(colnames(theta85Out)))
theta85Out <- cbind(meta$pid, theta85Out)

head(theta85Out)

##########
### Extract the highest proportion
## Attach to the paragraph IDs


### assigned topic in the 85 model:
theta85Out$max85 <- colnames(theta85Out[,2:86])[apply(
                                theta85Out[,2:86],1,which.max)] 

head(theta85Out)

#### Assigned paragraph topic in the 21 model:
theta21Out$max21 <- colnames(theta21Out[,2:22])[apply(
                                theta21Out[,2:22],1,which.max)] 

head(theta21Out)

## send out the paragraph-level topics:

paraTopics <- merge(x= theta21Out[,c("meta$pid", "max21")],
                    y=theta85Out[,c("meta$pid", "max85")],
                    by="meta$pid")


head(paraTopics)
tail(paraTopics)

paraTopics <- merge(x =meta,
                    y=paraTopics,
                    by.x="pid",
                    by.y="meta$pid")

head(paraTopics)
tail(paraTopics)

##write.csv(paraTopics,
##          file="M1to113DataAndTopics.csv")

###########
## 85-topic model topic correlations

cors.85 <-  topicCorr(mod.out.85,
                      method = c("simple"),
                      cutoff = 0.1,
                      verbose = TRUE)


class(cors.85)
attributes(cors.85)

plot(cors.85,
     vertex.size=.75,
     vertex.color="darkblue")

####################
## Digging into 
t72 <- findThoughts(mod.out.85,
                    texts=meta$cleanedtext,
                    topics=72,
                    n=3,
                    meta=meta)


plot(t72)
#### Plotting topics
########
attributes(prep.85)
table(prep.85$data$"income_level_iso3c")
## ADMN   AGG   HIC   LIC   LMC NOTST   UMC

plot(prep.85,
     topics=c(26),
     "numdate",
     method="continuous",
     moderator="income_level_iso3c",
     moderator.value="HIC",
     printlegend=TRUE)

plot(prep.85,
     topics=c(26),
     "numdate",
     method="continuous",
     moderator="income_level_iso3c",
     moderator.value="LIC",
     printlegend=FALSE,
     linecol="darkgreen",
     add=TRUE)
plot(prep.85,
     topics=c(26),
     "numdate",
     method="continuous",
     moderator="income_level_iso3c",
     moderator.value="LMC",
     linecol="blue",
     printlegend=FALSE,
     add=TRUE)
abline(h=0,lty=4,lwd=1,col="grey45")  # Put a dotted line on t
 
plot(prep.85)

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





