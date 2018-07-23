## script breaks the WTO full-text documents into paragraphs
## note though that it will also break "pararaphs" after a page break


library(readr)

readPath <- "~/Dropbox/WTO/data/txts/"


outPath <- "~/Dropbox/WTO/data/paras/"

 for(j in list.files(readPath, pattern="*.txt")){
    print(j)
    dat <- read_file(paste0(readPath, j))
    dat <- strsplit(dat, split='\n\n')
    dat <- as.data.frame(dat)
    
    for(i in 1:dim(dat)[1]){
        write(as.character(dat[i,1]),
              file=paste0(outPath, "para.", i, "doc", j ))
    }
}
