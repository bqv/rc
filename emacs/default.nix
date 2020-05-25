{ config, pkgs, lib, ... }:

{
  imports = lib.filter (p: lib.hasSuffix ".nix" p && builtins.baseNameOf p != "default.nix")
    (lib.mapAttrsToList (file: _: ./. + "/${file}") (builtins.readDir ./.));
}
