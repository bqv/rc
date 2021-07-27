{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.json-mode = {
    demand = true;
  };
}
