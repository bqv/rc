{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.explain-pause-mode = {
    demand = true;
    package = epkgs: epkgs.explain-pause-mode.overrideAttrs (_: {
      postInstall = ''
        find $out -iname '*.elc' -delete
      '';
    });
    require = [ "explain-pause-mode" ];
    config = ''
      (unless (fboundp 'explain-pause--seq-contains)
        (defun explain-pause--seq-contains (&rest r) nil))
      (explain-pause-mode)
    '';
  };
}
