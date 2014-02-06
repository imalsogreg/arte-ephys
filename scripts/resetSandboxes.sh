cd ~/Programming/arte-ephys
sudo rm -r .cabal-sandbox

cd ArteMaster
rm cabal.sandbox.config
cabal clean

cd ../ArteDecode
rm cabal.sandbox.config
cabal clean

cd ../ArteMockMWL
rm cabal.sandbox.config
cabal clean


