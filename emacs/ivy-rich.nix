{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.ivy-rich = {
    demand = true;
    after = [ "ivy" "counsel" ];
    config = ''
      (unless (fboundp 'ivy-rich-switch-buffer-transformer)
        (defun ivy-rich-switch-buffer-transformer (arg)
          arg))
      (ivy-rich-mode 1)
    '';
  };
}
