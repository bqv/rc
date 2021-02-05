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
  self = if builtins ? getFlake then builtins.getFlake (toString ./.) else flake.defaultNix;
  hostname = with builtins; head (split "\n" (readFile /etc/hostname));
  maybe = c: let result = builtins.tryEval c; in if result.success then result.value else {};
in rec { inherit flake-compat flake; inherit self; inputs = self.passthru.inputs // { inherit self; }; }
// maybe self // maybe (self.passthru or {})
// maybe self.defaultPackage.${system}
// maybe self.defaultPackage.${system}.config.nodes
// maybe self.defaultPackage.${system}.config.nodes.${hostname}.configuration
// maybe {
  lock = lock;
  inherit (self.defaultPackage.${system}.config.nodes.${hostname}.configuration._pkgs) pkgs lib;
  options = rec {
    deploy = self.defaultPackage.x86_64-linux.options;
    nodes = deploy.nodes.type.getSubOptions [];
    nixos = nodes.configuration.type.getSubOptions [];
    home = nixos.home-manager.users.type.getSubOptions [];
  };
}
