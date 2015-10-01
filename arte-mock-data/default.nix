{ mkDerivation, aeson, async, base, bytestring, cereal, errors
, network, optparse-applicative, pipes, pipes-rt, random, safe
, stdenv, stm, tetrode-ephys, text, time, transformers, vector
}:
mkDerivation {
  pname = "arte-mock-data";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring cereal errors network
    optparse-applicative pipes pipes-rt random safe stm tetrode-ephys
    text time transformers vector
  ];
  executableHaskellDepends = [
    aeson async base bytestring cereal errors network
    optparse-applicative pipes pipes-rt random safe stm tetrode-ephys
    text time transformers vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
