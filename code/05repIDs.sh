#!/bin/bash

## simple bash script to try to isolate those who are speaking
## by pulling "the representative(s) of" and then the next few words


##declare directory
 DIR=$1


for file in ${DIR}/*.txt;
do

    ## this looks for lines that start with "The representative(s)"
    ## then takes the next three words
    ## then keeps only those that are capitalized
    ## keeps output on the same line (repleace grep's new line with a space)
    ## finally prints a new line at the end of each file search

   printf "$file "
   egrep "^The representative of|The representatives of" $file | cut -d' ' -f4-6 | grep -oP "\w*[A-Z]+\w*" | tr '\n' " "
   echo
done

