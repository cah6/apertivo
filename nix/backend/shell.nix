{ lib ? (import <nixpkgs> {}).pkgs.lib
}:
let 
  pinnedPkgs = import ../pkgs-from-json.nix { json = ../nixos-18-09.json; };
  myPackages = (import ./backend.nix { withHoogle = false; } );

  projectDrvEnv = myPackages.backend.env.overrideAttrs (oldAttrs: rec {
    buildInputs = oldAttrs.buildInputs ++ [ 
      pinnedPkgs.haskellPackages.hlint
      pinnedPkgs.haskellPackages.cabal-install
      pinnedPkgs.haskellPackages.hsimport
      ];
    shellHook = ''
      export ES_PASSWORD=${builtins.readFile ./es_password.secret}
    '';
  });
in 
  projectDrvEnv