{ mkDerivation, ansi-terminal, async, base, stdenv, stm }:
mkDerivation {
  pname = "progress-meter";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ ansi-terminal async base stm ];
  homepage = "https://github.com/esoeylemez/progress-meter";
  description = "Live diagnostics for concurrent activity";
  license = stdenv.lib.licenses.bsd3;
}
