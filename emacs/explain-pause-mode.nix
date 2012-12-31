{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.explain-pause-mode = {
    demand = true;
    package = epkgs: epkgs.explain-pause-mode.overrideAttrs (_: {
      postInstall = ''
        find $out -iname '*.elc' -delete
      '';
    });
    require = [ "explain-pause-mode" ];
    config = ''
     ;(explain-pause-mode)
     ;(setq explain-pause-logging-default-log-location "/dev/log")
     ;(add-hook 'after-init-hook #'explain-pause-log-to-socket)
    '';
  };
}
