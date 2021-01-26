{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.eshell-git-prompt = {
    demand = true;
    config = ''
      (eshell-git-prompt-powerline)
    '';
  };
}
