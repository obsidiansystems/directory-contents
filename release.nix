builtins.mapAttrs (_: v: v.directory-contents) (import ./env.nix {})
