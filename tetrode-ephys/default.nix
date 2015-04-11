{ mkDerivation, async, base, binary, bytestring, cereal, containers
, data-binary-ieee754, lens, parsec, pipes, pipes-binary
, pipes-bytestring, pipes-parse, pipes-rt, safe, safecopy, stdenv
, stm, text, time, vector, vector-binary-instances
}:
mkDerivation {
  pname = "tetrode-ephys";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    async base binary bytestring cereal containers data-binary-ieee754
    lens parsec pipes pipes-binary pipes-bytestring pipes-parse
    pipes-rt safe safecopy stm text time vector vector-binary-instances
  ];
  license = stdenv.lib.licenses.bsd3;
}
