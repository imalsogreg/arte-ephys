
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

`tetrode-decoder` listens for spikes from a single tetrode, combines them with position data, maintains a model of the relationship between tetrode spiking an track position, and produces Bayesian estimate's of the current position at regular intervals given all of this information. `master-decoder` collects the estimates of multiple `tetrode-decoder`s into a single estimate.

tetrode-ephys
=============

Basic types for spikes an position, operations over spikes (e.g. cluster labeling), and file format parsing.

arte-time-sync
==============

C time server

arte-lib
===============

Time  serving a single machine.  Something like one arduino per computer, counting (and emitting, if master-clock) a clock signal.  Reports clock value via RPC. (need to test: is the serial port latency with arduino fast enough?)
