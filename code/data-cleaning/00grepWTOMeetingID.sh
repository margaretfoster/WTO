#!/bin/bash

## script to extract meeting IDs from WTO website


## takes saved html page
PAGE=$1

## returns list of meetingID= and the associated numbers

grep -io  MeetingID=[[:digit:]]* $1  > list.txt

cat list.txt | uniq list.txt > meetinglist.txt

## clean up
rm list.txt

echo done!
