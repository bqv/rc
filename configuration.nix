{ system ? builtins.currentSystem, ... }:

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
in rec { inherit flake-compat flake; self = flake.defaultNix; inputs = self.lib.inputs // { inherit self; }; }
// maybe flake.defaultNix // maybe (flake.defaultNix.lib or {})
// maybe flake.defaultNix.defaultPackage.${system}
// maybe flake.defaultNix.defaultPackage.${system}.config.nodes
// maybe flake.defaultNix.defaultPackage.${system}.config.nodes.${hostname}.configuration
// maybe { inherit (flake.defaultNix.defaultPackage.${system}.config.nodes.${hostname}.configuration._pkgs) pkgs lib; }
