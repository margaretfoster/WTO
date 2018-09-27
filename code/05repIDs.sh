#!/bin/bash

## simple bash script to try to isolate those who are speaking
## by pulling "the representative(s) of" and then the next few words


##declare directory
 DIR=$1


for file in ${DIR}/*.txt;
do

    ## this looks for lines that start with "The representative(s)" and which may have leading spaces
    ## then takes the next three words
    ## then keeps only those that are capitalized
    ## removes any stray commas
    ## keeps output on the same line (repleace grep's new line with a space)
    ## finally prints a new line at the end of each file search

    printf "$file "
    ischar=`egrep "^\s*The [Cc]hair|The Secretariat|The Director" $file`
    if [ "$ischar" != "" ]
    then
	printf "Chairman"
    else
	rep=`egrep "^\s*The representatives? of " $file`
	if [ "$rep" != "" ]
	then
	    echo "$rep" | cut -d' ' -f4-6 | tr ' ' '\n' | grep  "^[A-Z]" | tr -d "," | tr '\n' " "
	fi
	
    fi

    del=`egrep "^\s*The delegate? of " $file`
    if [ "$del" != "" ]
    then
	echo "$del" | cut -d' ' -f4-6 | tr ' ' '\n' | grep  "^[A-Z]" | tr -d "," | tr '\n' " "
    fi

    echo ""
done

