{ }:
let
  nixpkgsSets = import ./.ci/nixpkgs.nix;
  rp = import ./.ci/reflex-platform {};
  inherit (nixpkgsSets) nixos1809 nixos2003 unstable;
  inherit (nixos2003) lib;
  inherit (nixos2003.haskell.lib) doJailbreak dontCheck;
  sharedOverrides = self: super: {
    monoidal-containers = doJailbreak (self.callHackageDirect {
      pkg = "monoidal-containers";
      ver = "0.6.0.1";
      sha256 = "1caxq3jz2i4w3vm4qq1raa9f9avmyv4ghprx9x3xr0ga7cdy7x2m";
    } {});
  };
  ghcs = rec {
    ghc865 = nixos2003.haskell.packages.ghc865.override {
      overrides = sharedOverrides;
    };
    ghc884 = nixos2003.haskell.packages.ghc884.override {
      overrides = sharedOverrides;
    };
    ghcjs = rp.ghcjs;
  };
in
  lib.mapAttrs (_: ghc: ghc.callCabal2nix "directory-contents" ./. {}) ghcs
