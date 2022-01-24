{ pkgs ? (import ./reflex-platform {}).nixpkgs }:
let
  name = "directory-contents";
  platform = import ./reflex-platform {};
  inherit (platform.nixpkgs.haskell.lib) doJailbreak dontCheck;
  overrides = self: super: {
    coquina = self.callHackage "coquina" "0.1.0.0" {};
  };
  targets = ["ghc8_6" "ghc8_10"];

  ghcs = pkgs.lib.genAttrs targets (target: platform.${target}.override {
    inherit overrides;
  });
in pkgs.lib.mapAttrs
    (_: ghc:
      { ${name} = ghc.callCabal2nix name ./. {};
        ghc = ghc;
      }
    ) ghcs
