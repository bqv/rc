{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.flycheck-pos-tip = {
    demand = true;
    after = [ "flycheck" ];
    config = ''
      (flycheck-pos-tip-mode)
    '';
  };
}
