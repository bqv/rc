{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.cl = {
    package = lib.const null;
  };
}
