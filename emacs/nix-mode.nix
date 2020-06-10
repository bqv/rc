{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.nix-mode = {
    demand = true;
    config = ''
      (setq nix-repl-executable-args '("repl" "${pkgs.path}"))
      (setq nix-indent-function 'nix-indent-line)
    '';
  };
}
