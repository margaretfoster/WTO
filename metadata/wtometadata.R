## for scraping date metadata from the WTO

library("rvest")
library(magrittr)

tradeDM1<- read_html("https://docs.wto.org/dol2fe/Pages/FE_Browse/FE_B_004.aspx?StartDate=01%2f06%2f2011&EndDate=30%2f06%2f2018&SubjectId=46&SearchPage=FE_B_003&")
