#1/bin/bash

## script to download meeting minutes from WTO
## for meeting IDs in meetinglist.txt

LIST=$1

URL='https://docs.wto.org/dol2fe/Pages/FE_DownloadDocument.aspx?'

for id in $(cat $LIST) #breaks "meetinglist.txt" into "meeting" "list.txt"

#for id in $(cat meetinglist.txt)
do
   # echo $id

    NAME=${URL}${id}'&Language=1'

 curl -OLJ ${NAME} -H 'Connection: keep-alive' -H 'Cache-Control: max-age=0' -H 'Upgrade-Insecure-Requests: 1' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/66.0.3359.170 Safari/537.36' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8' -H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, br' -H 'Accept-Language: en-US,en;q=0.9,fr-CA;q=0.8,fr;q=0.7' -H 'Cookie: .ASPXANONYMOUS=ZXe-UeVhqhv1uZuj4WhqRUrckJ0cGBZ-wRwK2fJnL843XgNXkt4belifXa0PxqdYFWHaPs8QJRcCUqSPzgxm1gLSjmXI8CBaaAV7cm7Xl7d2345DU1wTRdb97eAG3cTcnqw2Rw2; ASP.NET_SessionId=qkoxkpzwgawzd05tyx42tsvf' --compressed

  ~/Research/AQAPRecruitment/code/download/randWait

 done
