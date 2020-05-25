{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.counsel-projectile = {
    demand = true;
    after = [ "projectile" "counsel" ];
    config = ''
      (counsel-projectile-mode t)
      (setq ivy-initial-inputs-alist nil)
    '';
  };
}
