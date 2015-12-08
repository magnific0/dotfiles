#!/bin/sh
for i in `seq 1 589`;
do
    j=`printf "%04d" $i`
    wget "http://reader.eblib.com/(S(w5de5rzmz4ixx0ldn5rfysfl))/GetPage.aspx?r=pdf&z=0&pg=$i&s=1376904481973" -O $j.pdf
done
    
