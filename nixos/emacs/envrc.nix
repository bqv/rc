{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.envrc = {
    demand = true;
    config = ''
      (envrc-global-mode)
      (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map)
      (define-key envrc-command-map (kbd "R") 'envrc-reload-all)
    '';
    systemDeps = with pkgs; [ direnv ];
  };
}
