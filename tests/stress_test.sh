#!/bin/sh

a=1

while [ $a -le 10 ]
do
   ./test.sh "$1" "$2" "$3" &
   a=`expr $a + 1`
done
