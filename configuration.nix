{ ... }:

let
  flake-compat = import (fetchTarball https://github.com/edolstra/flake-compat/archive/master.tar.gz);
  flake = flake-compat { src = ./.; };
  hostname = with builtins; head (split "\n" (readFile /etc/hostname));
  maybe = c: let result = builtins.tryEval c; in if result.success then result.value else {};
in { inherit flake-compat flake; self = flake.defaultNix; }
// maybe flake.defaultNix // maybe (flake.defaultNix.passthru or {})
// maybe flake.defaultNix.nixosConfigurations
// maybe flake.defaultNix.nixosConfigurations.${hostname}
// maybe flake.defaultNix.nixosConfigurations.${hostname}.config
// maybe { inherit (flake.defaultNix.nixosConfigurations.${hostname}.pkgs) lib; }
