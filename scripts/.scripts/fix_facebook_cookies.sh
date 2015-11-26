#!/bin/sh
cd ~/.local/share/uzbl
grep -v facebook cookies.txt > cookies.new.txt
mv cookies.new.txt cookies.txt
