{ config, pkgs, lib, ... }:

{
  imports = lib.filter (p: lib.hasSuffix ".nix" p && builtins.baseNameOf p != "default.nix")
    (lib.mapAttrsToList (file: _: ./. + "/${file}")  # strings to paths
      (lib.filterAttrs (f: _: f != ".dir-locals.el") # remove bad files
        (builtins.readDir ./.)));                    # enumerate folder
}
