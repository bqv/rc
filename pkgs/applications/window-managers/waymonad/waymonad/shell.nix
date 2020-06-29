let
  pkgs = import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; };
in pkgs.haskellPackages.shellFor { packages = p: [ p.waymonad ]; }
