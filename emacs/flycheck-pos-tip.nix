{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.flycheck-elsa = {
    demand = true;
    after = [ "flycheck" ];
    config = ''
      (flycheck-pos-tip-mode)
    '';
  };
}
