{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.slime-company = {
    demand = true;
    after = [ "slime" "company-mode" ];
  };
}
