## script breaks the WTO full-text documents into paragraphs
## note though that it will also break "pararaphs" after a page break

## working with the MeetingNotesCopy directory
## to keep titles the same

library(readr)

readPath <- "~/Dropbox/WTO/data/MeetingNotesCopy/"

outPath <- "~/Dropbox/WTO/data/paras2/"


list.files(readPath)

 for(j in list.files(readPath, pattern="*.txt")){
     print(j)
     dat <- read_file(paste0(readPath, j))
     ## splits at {2-3} digits followed by a period
     ## which is the style of how the WTO marks paragraphs
     ## in their meeting records
     dat <- strsplit(dat, split='([0-9]{1,3})(\\.)')
     dat <- as.data.frame(dat)
 
    for(i in 1:dim(dat)[1]){
        write(as.character(dat[i,1]),
              file=paste0(outPath, "para.", i, ".doc.", j ))
    }
 }
