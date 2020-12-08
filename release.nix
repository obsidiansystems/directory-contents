{ }:
let
  nixpkgsSets = import ./.ci/nixpkgs.nix;
  inherit (nixpkgsSets) nixos1809 nixos2003 unstable;
  inherit (nixos2003) lib;
  inherit (nixos2003.haskell.lib) doJailbreak dontCheck;
  dep-sum-overrides = self: super: {
    witherable = self.callHackageDirect {
      pkg = "witherable";
      ver = "0.3.5";
      sha256 = "1fqgz6gyzj3gnv7h5iy8wzycy0ywxjmi6jj9y5bnq7ayn2db90k9";
    } {};
    witherable-class = self.callHackageDirect {
      pkg = "witherable-class";
      ver = "0";
      sha256 = "1v9rkk040j87bnipljmvccxwz8phpkgnq6vbwdq0l7pf7w3im5wc";
    } {};
  };
  ghcs = rec {
    ghc865 = nixos2003.haskell.packages.ghc865;
    ghc884 = nixos2003.haskell.packages.ghc884.override {
    };
    ghc8102 = unstable.haskell.packages.ghc8102;
  };
in
  lib.mapAttrs (_: ghc: ghc.callCabal2nix "directory-contents" ./. {}) ghcs
