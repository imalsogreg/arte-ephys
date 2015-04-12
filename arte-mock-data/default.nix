{ mkDerivation, base, bytestring, cereal, errors, network
, optparse-applicative, pipes, pipes-rt, safe, stdenv
, tetrode-ephys, text, transformers, vector
}:
mkDerivation {
  pname = "arte-mock-data";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base bytestring cereal errors network optparse-applicative pipes
    pipes-rt safe tetrode-ephys text transformers vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
