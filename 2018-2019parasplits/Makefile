

## This  makefile to clean the data for the 2018-2019 WTO data cleaning

.PHONY : wtodata
wtodata : 01wtoparas.csv 02wtoNER.csv 03withfirstentity.csv

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


.PHONY : clean
clean :
	rm -f 01wtoparas.csv 02wtoNER.csv 03withfirstentity.csv
