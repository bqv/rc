{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.idle-highlight-mode = {
    demand = true;
    config = ''
      (add-hook 'prog-mode-hook 'idle-highlight-mode)
    '';
  };
}
