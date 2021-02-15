{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.map = {
    package = lib.const null;
  };
}
