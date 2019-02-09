{ lib ? (import <nixpkgs> {}).pkgs.lib
}:
let 
  pinnedPkgs = import ./pkgs-from-json.nix { json = ./nixos-18-09.json; };
  myPackages = (import ./backend.nix { withHoogle = false; } );

  # hie = (import (pinnedPkgs.fetchFromGitHub {
  #                  owner="domenkozar";
  #                  repo="hie-nix";
  #                  rev="1d7edd32779652437d8c1f28170e46c636f6523f";
  #                  sha256="1px146agwmsi0nznc1zd9zmhgjczz6zlb5yf21sp4mixzzbjsasq";
  #                }) {}).hie84;

  projectDrvEnv = myPackages.backend.env.overrideAttrs (oldAttrs: rec {
    buildInputs = oldAttrs.buildInputs ++ [ 
      pinnedPkgs.haskellPackages.hlint
      pinnedPkgs.haskellPackages.cabal-install
      pinnedPkgs.haskellPackages.hsimport
      # hie
      # pinnedPkgs.elasticsearch5
      # elmPkgs.elmPackages.elm
      ];
    shellHook = ''
    '';
  });
in 
  projectDrvEnv