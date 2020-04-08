

## This  makefile to clean the data for the 2018-2019 WTO data cleaning

.PHONY : wtodata
wtodata : 01wtoparas.csv 02wtoNER.csv

## Break the documents, add a counter, put into csv:
01wtoparas.csv  : ../2018-2019Data/Minutes/
	Rscript --vanilla  parasplitAdjust.R ../2018-2019Data/Minutes/ 01wtoparas.csv


### run NER:

02wtoNER.csv : 01wtoparas.csv 
	python adapted11NER.py 01wtoparas.csv 02wtoNER.csv


