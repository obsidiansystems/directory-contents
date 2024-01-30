{ pkgs ? import ./nixpkgs {} }:
let
  name = "directory-contents";
  overrides = self: super: {
    coquina = self.callHackageDirect {
      pkg = "coquina";
      ver = "0.1.0.1";
      sha256 = "1aajmxqaqqy71dq3113bs1x1yhjs4snc2zjh9x1fhfqlgz9yanmy";
    } {};
    which = self.callHackageDirect {
      pkg = "which";
      ver = "0.2.0.2";
      sha256 = "08krfgnjwn9791lwq6azvnj8wy0b1ivyndyhipnrip202vv30rl0";
    } {};
    tasty-hedgehog = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (self.callHackageDirect {
      pkg = "tasty-hedgehog";
      ver = "1.4.0.2";
      sha256 = "04kg2qdnsqzzmj3xggy2jcgidlp21lsjkz4sfnbq7b1yhrv2vbbc";
    } {}));
    hedgehog = pkgs.haskell.lib.doJailbreak (self.callHackageDirect {
      pkg = "hedgehog";
      ver = "1.4";
      sha256 = "1qxxhs720im0wpa5lsca0l8qsfmhbyphd1aq01nv96v29lgv795b";
    } {});

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
