{ pkgs ? import ./nixpkgs {} }:
let
  name = "directory-contents";
  overrides = self: super: {
    coquina = self.callHackageDirect {
      pkg = "coquina";
      ver = "0.1.0.1";
      sha256 = "1aajmxqaqqy71dq3113bs1x1yhjs4snc2zjh9x1fhfqlgz9yanmy";
    } {};
  };
  targets = ["ghc8107" "ghc981"];

  ghcs = pkgs.lib.genAttrs targets (target: pkgs.haskell.packages.${target}.override {
    inherit overrides;
  });
in pkgs.lib.mapAttrs
    (_: ghc:
      { ${name} = ghc.callCabal2nix name ./. {};
        ghc = ghc;
      }
    ) ghcs
