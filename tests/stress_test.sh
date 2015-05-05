#!/bin/sh

a=0

while [ $a -lt 100 ]
do
   curl -sL -w "%{http_code}\\n" 'http://'$1:$2'/'gameboard'?user_id='$3'&access_token=XXX' -o /dev/null
   a=`expr $a + 1`
done
