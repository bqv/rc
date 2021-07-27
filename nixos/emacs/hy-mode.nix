{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.hy-mode = {
    demand = true;
  };
}
