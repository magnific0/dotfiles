#!/bin/sh
for i in `seq 1 328`;
do
    j=`printf "%04d" $i`
    wget "http://babel.hathitrust.org/cgi/imgsrv/image?id=wu.89045786969;seq=$i;width=2048" -O $j.jpg
    tesseract -l eng $j.jpg $j.pdf hocr
    hocr2pdf -i $j.jpg -s -o $j.pdf < $j.pdf.html
done
/home/wacko/.scripts/pdfmerge *.pdf book_out.pdf
