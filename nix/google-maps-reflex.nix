{ mkDerivation, base, containers, data-default, dependent-map
, dependent-sum-template, fetchgit, ghcjs-dom, jsaddle, jsaddle-dom
, ref-tf, reflex, reflex-dom, stdenv, text
}:
mkDerivation {
  pname = "google-maps-reflex";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/cfraz89/google-maps-reflex";
    sha256 = "0fxvfjh1xnv4dl4dqnqszraczcsdbxh6mm49hh0c5lk94fkpk2qs";
    rev = "bd733662a0bb57927774a20419c46d9014eae897";
  };
  libraryHaskellDepends = [
    base containers data-default dependent-map dependent-sum-template
    ghcjs-dom jsaddle jsaddle-dom ref-tf reflex reflex-dom text
  ];
  homepage = "https://github.com/cfraz98/reflex-dom-google-maps#readme";
  license = stdenv.lib.licenses.bsd3;
}
