{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.company-box = {
    demand = true;
    after = [ "company" ];
    hook = [
      { company-mode-hook = "company-box-mode"; }
    ];
  };
}
