{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.flycheck = {
    demand = true;
    hook = [
      { prog-mode = "flycheck-mode"; }
    ];
  };
}
