#!/bin/sh
gs	-q -dNOPAUSE -dBATCH -dSAFER \
	-sDEVICE=pdfwrite \
	-dCompatibilityLevel=1.3 \
	-dPDFSETTINGS=/screen \
	-dEmbedAllFonts=true \
	-dSubsetFonts=true \
	-dColorImageDownsampleType=/Bicubic \
	-dColorImageResolution=300 \
	-dGrayImageDownsampleType=/Bicubic \
	-dGrayImageResolution=300 \
	-dMonoImageDownsampleType=/Bicubic \
	-dMonoImageResolution=300 \
	-sOutputFile="$1_compressed.pdf" \
	"$1"
