{ lib ? (import <nixpkgs> {}).pkgs.lib
}:
let 
  pinnedPkgs = import ../pkgs-from-json.nix { json = ../nixos-18-09.json; };
  myPackages = (import ./backend.nix { withHoogle = false; } );
  # all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  projectDrvEnv = myPackages.backend.env.overrideAttrs (oldAttrs: rec {
    buildInputs = oldAttrs.buildInputs ++ [ 
      pinnedPkgs.haskellPackages.hlint
      pinnedPkgs.haskellPackages.cabal-install
      pinnedPkgs.haskellPackages.hsimport
      # all-hies.versions.ghc843
      ];
    # export HIE_HOOGLE_DATABASE="$NIX_GHC_LIBDIR/../../share/doc/hoogle/index.html"
    shellHook = ''
      export ES_PASSWORD=${builtins.readFile ./es_password.secret}
    '';
  });
in 
  projectDrvEnv