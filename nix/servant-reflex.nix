# Not in nix packages yet, so need to manually include from github
{ mkDerivation, base, bytestring, case-insensitive, containers
, data-default, exceptions, fetchgit, ghcjs-dom, http-api-data
, http-media, jsaddle, mtl, network-uri, reflex, reflex-dom-core
, safe, servant, servant-auth, stdenv, string-conversions, text
, transformers
}:
mkDerivation {
  pname = "servant-reflex";
  version = "0.3.3";
  src = fetchgit {
    url = "https://github.com/imalsogreg/servant-reflex";
    sha256 = "127p8pa8zzbagl28558aik38bwnjkv54mh835qzwks0gmvzaif5s";
    rev = "2bd57a1124fb348fe58eab60947812621d3c2278";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers data-default exceptions
    ghcjs-dom http-api-data http-media jsaddle mtl network-uri reflex
    reflex-dom-core safe servant servant-auth string-conversions text
    transformers
  ];
  description = "Servant reflex API generator";
  license = stdenv.lib.licenses.bsd3;
}
