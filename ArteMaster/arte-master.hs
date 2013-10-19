name:                     arte-master
version:                  0.1
homepage:                 http://github.com/ImAlsoGreg/arte-ephys
synopsis:                 A soft-realtime tetrode recording system
description:              
        arte-ephys is a soft-realtime tetrode recording system.
        It collects data in small (~1ms) buffers for filtering,
        spike detection, network transfer, viewing, and
        online feedback. arte-master is the main hub through
        which users interact with the system. (although some UI
        happens in the data viewers too)
category:                 Science
license:                  GPL-3
license-file:             LICENSE
author:                   Greg Hale
maintainer:               imalsogreg@gmail.com
cabal-version:            >= 1.8
build-type:               Simple

executable arte-master
           build-depends:  gtk2hs, mtl, stm >= 2.4
           main-is:        Master.hs
           ghc-options:    -Wall
           hs-source-dirs: src