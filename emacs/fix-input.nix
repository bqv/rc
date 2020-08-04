{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.fix-input = {
    demand = true;
    config = ''
      (fix-input "british" "programmer-dvorak" "uk-programmer-dvorak")
      (setq default-input-method "uk-programmer-dvorak")
    '';
  };
}
