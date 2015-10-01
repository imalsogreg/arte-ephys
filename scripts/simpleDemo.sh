#!/bin/bash

DATADIR="$HOME/data/caillou/112812clip2"
STARTTIME="4492"

echo "Starting arte-time-sync"
../arte-time-sync/time &

echo "Starting mock data"
../arte-mock-data/dist/build/arte-mock-data/arte-mock-data --format ArteJSON --localport 5000 --destport 5001 --startTime 4492 --file anything.pfake &

echo "Starting trode decoder"
../arte-decoder/dist/build/trode-decoder/trode-decoder --ttDir $DATADIR/1028/ --timeSyncPort 6080 --posport 5001 --spikeformat ArteJSON

