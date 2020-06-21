{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.dante = {
    demand = true;
    after = [ "haskell-mode" ];
    hook = [
      { haskell-mode-hook = "dante-mode"; }
    ];
  };
}
