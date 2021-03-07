{ config, pkgs, lib, ... }:

{
  imports = lib.filter (p: lib.hasSuffix ".nix" p && builtins.baseNameOf p != "default.nix")
    (lib.mapAttrsToList (file: _: ./. + "/${file}")  # strings to paths
      (lib.filterAttrs (f: _: f != ".dir-locals.el") # remove bad files
        (builtins.readDir ./.)));                    # enumerate folder

  emacs.loader.scraps = {
    package = lib.const null;
    script = epkgs: ''
      ${builtins.readFile ./scraps+bufmgmt.el}
      ${builtins.readFile ./scraps+misc.el}
      ${builtins.readFile ./scraps+shell.el}
    '';
  };
}
