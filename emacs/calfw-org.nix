{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.calfw-org = {
    demand = true;
    after = [ "org" "calfw" ];
    config = ''
      (setq calendar-week-start-day 1)
    '';
  };
}
