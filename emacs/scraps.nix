{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.scraps = {
    package = lib.const null;
    script = epkgs: ''
      ${builtins.readFile ./scraps+bufmgmt.el}
      ${builtins.readFile ./scraps+misc.el}
      ${builtins.readFile ./scraps+nix.el}
      ${builtins.readFile ./scraps+shell.el}
      ${builtins.readFile ./scraps+web.el}
    '';
  };
}
