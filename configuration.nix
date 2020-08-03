{ ... }:

let
  flake-compat = import (fetchTarball https://github.com/edolstra/flake-compat/archive/master.tar.gz);
  flake = flake-compat { src = ./.; };
  hostname = with builtins; head (split "\n" (readFile /etc/hostname));
in flake.defaultNix.nixosConfigurations.${hostname}.config
