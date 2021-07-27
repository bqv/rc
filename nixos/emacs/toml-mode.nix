{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.toml-mode = {
    demand = true;
  };
}
