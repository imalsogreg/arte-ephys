{ mkDerivation, aeson, async, base, base64-bytestring, bytestring
, cereal, containers, directory, either, errors, filepath, lens
, network, optparse-applicative, stdenv, stm, tetrode-ephys, text
, time, transformers, vector
}:
mkDerivation {
  pname = "arte-lib";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    aeson async base base64-bytestring bytestring cereal containers
    directory either errors filepath lens network optparse-applicative
    stm tetrode-ephys text time transformers vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
