#!/bin/sh

a=1

while [ $a -le 100 ]
do
   curl -sL -w "$a. %{http_code}\\n" 'http://'$1:$2'/'gameboard'?user_id='$3'&access_token=XXX' -o /dev/null
   a=`expr $a + 1`
done
