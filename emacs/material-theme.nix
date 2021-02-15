{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.material-theme = {
    demand = true;
  };
}
