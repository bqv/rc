{ ... }:

let
  flake-compat = import (fetchTarball https://github.com/edolstra/flake-compat/archive/master.tar.gz);
  flake = flake-compat { src = ./.; };
  hostname = with builtins; head (split "\n" (readFile /etc/hostname));
in flake.defaultNix // (flake.defaultNix.passthru or {})
// flake.defaultNix.nixosConfigurations
// flake.defaultNix.nixosConfigurations.${hostname}
// flake.defaultNix.nixosConfigurations.${hostname}.config
// { inherit (flake.defaultNix.nixosConfigurations.${hostname}.pkgs) lib; }
