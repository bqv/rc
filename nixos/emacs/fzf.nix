{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.fzf = {
    demand = true;
  };
}
