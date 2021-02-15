{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.android-env = {
    demand = true;
  };
}
