{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.bufler = {
    demand = true;
    config = ''
      (if (boundp 'bufler-vc-remote)
          (setq bufler-vc-remote nil)
        (setq bufler-columns (remove "VC" bufler-columns)))
    '';
  };
}
