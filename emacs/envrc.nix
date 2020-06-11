{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.envrc = {
    demand = true;
    config = ''
      (envrc-global-mode)
    '';
    systemDeps = with pkgs; [ direnv ];
  };
}
