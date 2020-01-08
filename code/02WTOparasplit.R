#!/usr/bin/env Rscript

## This code allows to run the script from the commandline
## Takes two arguments:
## [1] location of the input
## [2] location for the ouput

## pass in datapath and output path
args = commandArgs(trailingOnly=TRUE)

## make sure at least one argument:

if (length(args)==0) {
    stop("Must provide data and output path",
         call.=FALSE)
} else if (length(args)==1) {
  # default output path
  args[2] = "../"
}

## script breaks the WTO full-text documents into paragraphs
## note though that it will also break "pararaphs" after a page break

## working with the MeetingNotesCopy directory
## to keep titles the same

## load libraries:prelimsetup.R
source('prelimsetup.R')

## take the passed-in directory locations:
## Assumption is that the first

readPath <- args[1]
outPath <- args[2]


list.files(readPath)

 for(j in list.files(readPath, pattern="*.txt")){
     print(j)
     dat <- read_file(paste0(readPath, j))
     ## splits at {1-3} digits followed by a period
     ## which is the style of how the WTO marks paragraphs
     ## in their meeting records
     dat <- strsplit(dat, split='([0-9]{1,3})(\\.)')
     dat <- as.data.frame(dat)
 
    for(i in 1:dim(dat)[1]){
        write(as.character(dat[i,1]),
              file=paste0(outPath, "para.", i, ".doc.", j ))
    }
 }
