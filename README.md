Installation
============

C dependencies:
  - OpenGL
  - llvm
  - nvidia drivers
  - glut
  - glu
  - cairo
  - pango
  - ffmpeg
  - OpenCV
  - [flycapture2](http://www.ptgrey.com/flycapture-sdk)
  
Special Haskell dependencies:
  - [ffmpeg-light](http://github.com/acowley/ffmpeg-light)
  - [tetrode-ephys](http://github.com/imalsogreg/tetrode-ephys)

Installation is tricky on Ubuntu because of the ffmpeg fork, but hopefully this will be better in Ubuntu 15.04.

Instructions for Ubuntu 14.04:

  1. `sudo apt-get install libcairo2-dev llvm-dev` TODO fix up
  2. `git clone git@github.com:imalsogreg/arte-ephys`
  3. `cabal sandbox init`
  4. [#ffmpeg-light] TODO
  5. `cabal install`
  
arte-ephys
==========

[![Build Status](https://travis-ci.org/imalsogreg/arte-ephys.svg?branch=master)](https://travis-ci.org/imalsogreg/arte-ephys)

Soft realtime electrophysiology setup

This is the beginning of a rewrite of the c/c++ work at arte-ephys.googlecode.com.  I want to try to write the backend in ocaml, and the visualization, extraction, etc in ocaml and haskell.  Also want to use json as the network data format, disk format, config file format.

Hope to rely on forthcoming ocaml-tetrode-ephys library (and haskell-tetrode-ephys?) for representing data types.  Expecting to rely heavily on zmq for routing RPC and data.


arte-master
===========

Start various components on various computers.  Hub of user input.  Either all data is displayed by a single arte-master GUI, or arte-master controls launching and interacting with display ports in other processes/on other machines (not sure which makes more sense)

arte-base
=========

Library of functions used in multiple arte applications (but not general to all ephys-stuff - that level of stuff belongs in ocaml-tetrode-ephys).  
  * Serialization layer
  * RPC
  * Configuration reading

arte-backend
============

Interface with NiDAQ card, take in small data buffers(, demultiplex), filter data in LFP-band and export LFP data; filter in spike band, threshold-detect spike, export spikes; listen for messages (start/stop recording, reset clock).


tracker
=======

Use cameras to track position.


arte-lfp
========

Provide a gui component for drawing LFP signals.  Also, special processing on top of tetrode-ephys library's lfp functions.


arte-cluster
============

Provide gui component for displaying streaming in spike data and displaying it in cluster plots.  Also some realtime feedback functions.  (How much is done here?  How much in ocaml-tetrode-ephys?)


arte-timeserver
===============

Time server serving a single machine.  Something like one arduino per computer, counting (and emitting, if master-clock) a clock signal.  Reports clock value via RPC. (need to test: is the serial port latency with arduino fast enough?)
