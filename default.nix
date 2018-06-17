{}:

# let
#   extendHaskellPackages = import ../dapp-tools/nix/haskell.nix;
#   custom-overlay = self: super: {
#     haskellPackages = extendHaskellPackages {
#       inherit (super) haskellPackages lib;
#       pkgs = super;
#     };
#   };
#   nixpkgsFunc = opts:
#     import ./reflex-platform/nixpkgs
#       (opts // { overlays = opts.overlays ++ [custom-overlay]; });

(
  import ./reflex-platform {
    globalExtensions = self: super: {
      ethjet = self.callPackage (import ../dapptools/libethjet) {};
      solc = self.callPackage (
        (import ../dapptools/nix/packages/solc-versions.nix).solc_0_4_23
      ) {};
    };
    haskellExtensions = nixpkgs: self: super: {
      inherit (
        import ../dapptools/nix/haskell.nix
          { pkgs = nixpkgs; lib = nixpkgs.lib; }
          self super
      ) hevm restless-git megaparsec ethjet;
    };
  }
).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  android.frontend = {
    executableName = "frontend";
    applicationId = "org.example.frontend";
    displayName = "Example Android App";
  };

  ios.frontend = {
    executableName = "frontend";
    bundleIdentifier = "org.example.frontend";
    bundleName = "Example iOS App";
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };
})
