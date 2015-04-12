{ mkDerivation, array, arte-lib, async, base, bytestring, cereal
, cmdargs, containers, data-clist, directory, gloss, lens, network
, pipes, pipes-rt, safe, stdenv, stm, tetrode-ephys
, tetrode-graphics, text, time, vector
}:
mkDerivation {
  pname = "arte-decoder";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    array arte-lib async base bytestring cereal cmdargs containers
    data-clist directory gloss lens network pipes pipes-rt safe stm
    tetrode-ephys tetrode-graphics text time vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
