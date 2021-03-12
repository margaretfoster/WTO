

## This script to merge the extracted paragraphs
## and the identified entities
## creates 03data.csv                                                                                                              

## load libraries:prelimsetup.R                                                                                                                               
source('../code/prelimsetup.R')

## Pass in the base data:
args = commandArgs(trailingOnly=TRUE)

## Pass in directory locations via the command line
##paradata <- args[1] ## data
paradata <- read.csv(01wtoparas.csv)

##nerdata <- args[2] ## output name
nerdata <- read.csv()


