{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.flycheck-haskell = {
    demand = true;
  };
}
