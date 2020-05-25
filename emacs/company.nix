{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.company = {
    demand = true;
    diminish = [ "company-mode" ];
    hook = [
      { prog-mode = "company-mode"; }
    ];
    config = ''
      (setq company-tooltip-align-annotations t
            company-minimum-prefix-length 1)
    '';
  };
}
