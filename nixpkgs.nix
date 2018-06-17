let
  extendHaskellPackages = import ../dapp-tools/nix/haskell.nix;
  custom-overlay = self: super: {
    haskellPackages = extendHaskellPackages {
      inherit (super) haskellPackages lib;
      pkgs = super;
    };
  };
in (import ./reflex-platform {
  nixpkgsFunc = opts:
    import ./reflex-platform/nixpkgs
      (opts // { overlays = opts.overlays ++ [custom-overlay]; });
})
