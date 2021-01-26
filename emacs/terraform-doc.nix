{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.terraform-doc = {
    demand = true;
    after = [ "terraform-mode" ];
  };
}
