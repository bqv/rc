{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.meson-mode = {
    demand = true;
  };
}
