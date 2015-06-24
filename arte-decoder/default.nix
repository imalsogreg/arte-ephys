{ mkDerivation, array, arte-lib, async, base, bytestring, cereal
, containers, directory, filepath, gloss, ip-quoter, lens, network
, optparse-applicative, pipes, pipes-rt, safe, stdenv, stm
, tetrode-ephys, tetrode-graphics, text, time, transformers, vector
}:
mkDerivation {
  pname = "arte-decoder";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    array arte-lib async base bytestring cereal containers directory
    filepath gloss ip-quoter lens network optparse-applicative pipes
    pipes-rt safe stm tetrode-ephys tetrode-graphics text time
    transformers vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
