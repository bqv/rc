{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.misc-cmds = {
    demand = true;
    package = lib.const null;
  };
}
