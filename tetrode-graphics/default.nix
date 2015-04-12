{ mkDerivation, base, containers, gloss, JuicyPixels, lens, stdenv
, tetrode-ephys, vector
}:
mkDerivation {
  pname = "tetrode-graphics";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    base containers gloss JuicyPixels lens tetrode-ephys vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
