
[![Build Status](https://travis-ci.org/imalsogreg/arte-ephys.svg?branch=master)](https://travis-ci.org/imalsogreg/arte-ephys)

Installation
============

C dependencies:
  - OpenGL
  - llvm
  - glut
  - glu
  - cairo
  - pango
  - ffmpeg


Instructions for Ubuntu 14.04:

  1. `sudo apt-get install libcairo2-dev llvm-dev`
  2. `git clone git@github.com:imalsogreg/arte-ephys`
  3. TODO sandbox setup script

arte-decoder
============

Soft realtime electrophysiology setup

tetrode-ephys
=============

arte-time-sync
==============

C time server

arte-lib
===============

Time  serving a single machine.  Something like one arduino per computer, counting (and emitting, if master-clock) a clock signal.  Reports clock value via RPC. (need to test: is the serial port latency with arduino fast enough?)
