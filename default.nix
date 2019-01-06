{ reflex-platform ? import ./reflex-platform {} }:

reflex-platform.project ({ pkgs, ... }: {
  useWarp = true;

  packages = {
    common = ./common;
    frontend = ./frontend;
  };

  shells = {
    ghc = ["common" "frontend"];
    ghcjs = ["common" "frontend"];
  };

  overrides = self: super: {
    http-media = pkgs.haskell.lib.dontCheck super.http-media;
    servant = pkgs.haskell.lib.dontCheck super.servant;
    # doctest = self.callPackage ./nix/doctest.nix { };
    servant-reflex = self.callPackage ./nix/servant-reflex.nix { };
  };

  shellToolOverrides = ghc: super: {
    cabal-install = pkgs.haskellPackages.cabal-install;
    hlint = pkgs.haskellPackages.hlint;
    hie = (import (pkgs.fetchFromGitHub {
                   owner="domenkozar";
                   repo="hie-nix";
                   rev="1d7edd32779652437d8c1f28170e46c636f6523f";
                   sha256="1px146agwmsi0nznc1zd9zmhgjczz6zlb5yf21sp4mixzzbjsasq";
                 }) {}).hie84;
  };
})
