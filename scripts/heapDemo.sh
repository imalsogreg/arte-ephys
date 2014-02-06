#!/bin/bash

DATADIR="/home/greghale/Data/caillou/112812clip2"
STARTTIME="4492"

echo "Starting ArteMaster"
.cabal-sandbox/bin/arte-master -s=2 +RTS -h &

echo "Starting ArteDecode"
.cabal-sandbox/bin/arteDecode +RTS -h &

echo "Starting mock data"
.cabal-sandbox/bin/arteMockSpikes -i=false --startexperimenttime=$STARTTIME --basedirectory=$DATADIR --searchdepth=2 +RTS -h &
