{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.gh-notify = {
    demand = true;
    after = [ "magit" "forge" ];
    config = ''
    '';
  };
}
