{ ... }:

let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  locked-state = lock.nodes.flake-compat.locked;
  flake-compat = import (fetchTarball (
    if builtins.pathExists ./flake.lock && builtins.hasAttr "flake-compat" lock.nodes
    then {
      url = "https://github.com/edolstra/flake-compat/archive/${locked-state.rev}.tar.gz";
      hash = locked-state.narHash;
    }
    else https://github.com/edolstra/flake-compat/archive/master.tar.gz));
  flake = flake-compat { src = ./.; };
  hostname = with builtins; head (split "\n" (readFile /etc/hostname));
  maybe = c: let result = builtins.tryEval c; in if result.success then result.value else {};
in { inherit flake-compat flake; self = flake.defaultNix; }
// maybe flake.defaultNix // maybe (flake.defaultNix.passthru or {})
// maybe flake.defaultNix.nixosConfigurations
// maybe flake.defaultNix.nixosConfigurations.${hostname}
// maybe flake.defaultNix.nixosConfigurations.${hostname}.config
// maybe { inherit (flake.defaultNix.nixosConfigurations.${hostname}.pkgs) lib; }
