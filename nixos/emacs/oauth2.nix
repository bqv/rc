{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.oauth2 = {
    demand = true;
  };
}
