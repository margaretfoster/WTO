import csv
import os, glob

def makeCSV(flist):
    with open('texts.csv', 'w') as csvfile:
        csvw = csv.writer(csvfile)
        for f in flist:
            with open(f, 'r') as infile:
                instr=''
                for line in infile:
                    instr = instr + line
                    pass
                print(instr)
                csvw.writerow([instr])
                pass
            pass
        pass
    pass

from glob import glob

filelist = [path(f).abspath() for f in glob("/Users/Promachos/Dropbox/WTO/data/paras/*.txt")]

## laptop path:
##filelist = ["/Users/Promachos/Dropbox/WTO/data/paras/" +
##          str(i) for i in range(1, 7505)] ## 82 documents
##filelist=[f + ".txt" for f in filelist] ## 

makeCSV(filelist)

