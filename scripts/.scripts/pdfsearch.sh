#!/bin/sh
pdfgrep -i -n -C 30 $@ *.pdf
