{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "doctest";
  version = "0.15.0";
  src = ./doctest;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/sol/doctest#readme";
  description = "Test interactive Haskell examples";
  license = stdenv.lib.licenses.mit;
}
