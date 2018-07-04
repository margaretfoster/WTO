#1/bin/bash

## takes a list of downloaded htm files

LIST=$1

for line in $LIST
##run:
            ../00grepWTOMeetingID.sh ../$line
            
            ../01downloadMeetings.sh ../meetinglist.txt 

            
