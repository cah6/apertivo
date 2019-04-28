{ reflex-platform ? import ./nix/reflex-platform.nix {}
, withHoogle ? false
}:
reflex-platform.project ({ pkgs, ... }: {

  inherit withHoogle;
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
    lens-aeson = pkgs.haskell.lib.dontCheck super.lens-aeson;
    # jsaddle-dom = self.callPackage ./nix/jsaddle-dom.nix { };
    servant-reflex = self.callPackage ./nix/servant-reflex.nix { };
    google-maps-reflex = self.callPackage ./nix/google-maps-reflex.nix { };
  };

  # shellToolOverrides = ghc: super: {
  #   cabal-install = pkgs.haskellPackages.cabal-install;
  #   hlint = pkgs.haskellPackages.hlint;
  # };
})
