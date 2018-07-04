## Workthrough webscraping reminder
## for WTO documents

rm(list=ls())

loadPkg=function(toLoad){
  for(lib in toLoad){
  if(! lib %in% installed.packages()[,1])
    { install.packages(lib, repos='http://cran.rstudio.com/') }
  suppressMessages( library(lib, character.only=TRUE) ) }}

packs=c('rvest')
loadPkg(packs)


