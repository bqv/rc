{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.yasnippet = {
    demand = true;
  };
}
