{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.fish-completion = {
    demand = true;
    config = ''
      (global-fish-completion-mode)
    '';
    systemDeps = with pkgs; [ fish ];
  };
}
