#!/bin/bash


## script to pull date metadata
## out of the txt meetings

## feed in location of meeting notes:

DIR=$1

## for file in directory
## echo the file name
## echo line that starts with "note on the meeting of"

for file in ${DIR}/*.txt;
do
    echo -n $file "; " ##-n makes no new line  
    grep '^NOTE' $file | tr -d '\n' ## greps the date and supresses newline behavior
    echo ## prints a newline at the end of each entry, even if the date isn't there
done
