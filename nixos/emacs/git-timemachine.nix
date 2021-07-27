{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.git-timemachine = {
    demand = true;
  };
}
