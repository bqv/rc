{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.company-terraform = {
    demand = true;
    after = [ "terraform-mode" "company" ];
  };
}
