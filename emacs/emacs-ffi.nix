{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.emacs-ffi = {
    demand = true;
  };
}
