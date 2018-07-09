import csv
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

filelist = ["/Users/Promachos/Dropbox/WTO/data/" +
          str(i) for i in range(1, 82)] ## 82 documents

filelist=[f + ".txt" for f in filelist] ## 

makeCSV(filelist)

