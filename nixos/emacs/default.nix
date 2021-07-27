{ config, pkgs, lib, ... }:

{
  imports = lib.filter (p: lib.hasSuffix ".nix" p && builtins.baseNameOf p != "default.nix")
    (lib.mapAttrsToList (file: _: ./. + "/${file}")  # strings to paths
      (lib.filterAttrs (f: _: f != ".dir-locals.el") # remove bad files
        (builtins.readDir ./.)));                    # enumerate folder

  emacs.loader.scraps = {
    package = lib.const null;
    script = epkgs: ''
      (defun bqv/load-scraps ()
        ${builtins.readFile ./scraps+bufmgmt.el}
        ${builtins.readFile ./scraps+misc.el}
        ${builtins.readFile ./scraps+shell.el}
        t)
      (add-to-list 'emacs-startup-hook #'bqv/load-scraps)
    '';
  };
}
