#!/bin/bash

MOCKDATA=`pidof arteMockMWL`
kill -9 $MOCKDATA

DECODER=`pidof arteDecode`
kill -9 $DECODER

MASTERP=`pidof arte-master`
kill -9 $MASTERP



