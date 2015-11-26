#!/bin/sh
#echo "awful.layout.set( awful.layout.suit.floating )" | awesome-client
#export LD_PRELOAD=/usr/lib/libstdc++.so.6
export MATLAB_JAVA=/usr/lib/jvm/default-runtime/
nohup ~/opt/MATLAB/R2015a/bin/matlab -desktop 2>&1 > /dev/null &
