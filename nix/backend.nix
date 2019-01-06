{ withHoogle ? false
}:
let
  pinnedPkgs = import ./pkgs-from-json.nix { json = ./nixos-18-09.json; };

  pinnedHaskellPkgs = pinnedPkgs.haskellPackages;

  customHaskellPackages = pinnedHaskellPkgs.override (old: {
    overrides = pinnedPkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
      backend = self.callCabal2nix "backend" ../backend { };
      common = self.callCabal2nix "common" ../common { };
    });
  });

  hoogleAugmentedPackages = import ./toggle-hoogle.nix { withHoogle = withHoogle; input = customHaskellPackages; };

in
  { backend = hoogleAugmentedPackages.backend;
  }