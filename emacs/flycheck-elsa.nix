{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.flycheck-elsa = {
    demand = true;
    after = [ "elsa" "flycheck" ];
    config = ''
      (flycheck-elsa-setup)
    '';
  };
}
