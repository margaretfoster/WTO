

## This  makefile to clean the data for the 2018-2019 WTO data cleaning
## Note that adapted11NER.py really needs to run on a computer without python3 downloaded, or there will be problems with NLTK
## (MJF workflow: that means the vcm 12266 machine)

 
.PHONY : wtodata
wtodata : 01wtoparas.csv 02wtoNER.csv 03withfirstentity.csv WTO_TD_NER_Data.csv

## Break the documents, add a counter, put into csv:
01wtoparas.csv  : ../2018-2019Data/Minutes/
	Rscript --vanilla  parasplitAdjust.R ../2018-2019Data/Minutes/ 01wtoparas.csv


### run NER:

02wtoNER.csv : 01wtoparas.csv 
	python adapted11NER.py 01wtoparas.csv 02wtoNER.csv

03withfirstentity.csv : 02wtoNER.csv
	Rscript --vanilla findfirstspeaker.R 02wtoNER.csv 03withfirstentity.csv


WTO_TD_NER_Data.csv : 03withfirstentity.csv
	Rscript --vanilla 04dateMetaData.R 03withfirstentity.csv WTO_TD_NER_Data.csv

WTO_TD_NER_Sentences.csv: WTO_TD_NER_Data.csv
ex	Rscript --vanilla 05sentencesConvert.R WTO_TD_NER_Data.csv WTO_TD_NER_Sentences.csv

.PHONY : clean
clean :
	rm -f 01wtoparas.csv 02wtoNER.csv 03withfirstentity.csv
