{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.flycheck-jest = {
    demand = true;
  };
}
