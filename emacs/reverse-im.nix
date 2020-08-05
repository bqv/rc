{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.reverse-im = {
    demand = true;
    config = ''
      (setq reverse-im-input-methods '("british"));
    '';
  };
}
