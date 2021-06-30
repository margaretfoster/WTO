## Experimenting with Stanford NLP for R
## Rather than the Bash cleaning

rm(list=ls())

## read in data:

##install.packages("readtext")

library(readtext)

dataPath="../2018-2019Data/paras/"


rawData <- readtext(dataPath)

install.packages("rJava", dependencies=TRUE)
install.packages("coreNLP", dependencies=TRUE)
