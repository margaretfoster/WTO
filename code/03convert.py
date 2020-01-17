#!/usr/bin/python

## script that takes a list of txt files
## and moves them into a csv
## takes write path as input
import sys
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
                csvw.writerow([f, instr]) ## writes file name in col1 and content in col2
                pass
            pass
        pass
    pass

from glob import glob

## path on VCM:

loc = sys.argv[1]

filelist = [os.path.abspath(f) for f in glob(loc)] ## path to data

## laptop path:
##filelist = ["/Users/Promachos/Dropbox/WTO/data/paras/" +
##          str(i) for i in range(1, 7505)] ## 82 documents
##filelist=[f + ".txt" for f in filelist] ## 

makeCSV(filelist)

