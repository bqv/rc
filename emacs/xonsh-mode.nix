{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.xonsh-mode = {
    demand = true;
  };
}
