{ mkDerivation, async, base, containers, stdenv, stm }:
mkDerivation {
  pname = "progress-meter";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ async base containers stm ];
  homepage = "https://github.com/esoeylemez/progress-meter";
  description = "Live diagnostics for concurrent activity";
  license = stdenv.lib.licenses.bsd3;
}
