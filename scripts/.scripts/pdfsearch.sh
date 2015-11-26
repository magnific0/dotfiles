#!/bin/bash
pdfgrep -i -n -C 30 $@ *.pdf
