{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.flycheck = {
    demand = true;
    hook = [
      { prog-mode-hook = "flycheck-mode"; }
    ];
  };
}
