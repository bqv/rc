{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.emms-mode-line-cycle = {
    demand = true;
    after = [ "emms" ];
    config = ''
      nil
    '';
  };
}
