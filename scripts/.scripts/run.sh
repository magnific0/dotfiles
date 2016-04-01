#!/bin/sh

source ~/.bashrc
[ -z $RUNNER ] || { eval "$RUNNER"; exit 0; }

type bashrun  > /dev/null 2>&1 && { bashrun; exit 0; }

type krunner  > /dev/null 2>&1 && { krunner; exit 0; }

type xfrun4   > /dev/null 2>&1 && { xfrun4; exit 0; }

type gmrun    > /dev/null 2>&1 && { gmrun; exit 0; }

type yeganesh > /dev/null 2>&1 && { yeganesh; exit 0; }

type yegonesh > /dev/null 2>&1 && { yegonesh; exit 0; }

