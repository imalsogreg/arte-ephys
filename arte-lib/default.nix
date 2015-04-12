{ mkDerivation, aeson, async, base, base64-bytestring, bytestring
, cereal, containers, directory, either, filepath, lens, network
, stdenv, stm, tetrode-ephys, text, time, transformers, vector
, yaml
}:
mkDerivation {
  pname = "arte-lib";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    aeson async base base64-bytestring bytestring cereal containers
    directory either filepath lens network stm tetrode-ephys text time
    transformers vector yaml
  ];
  license = stdenv.lib.licenses.bsd3;
}
