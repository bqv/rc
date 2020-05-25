{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.vterm = {
    demand = true;
    package = epkgs: with epkgs; [ vterm emacs-libvterm ];
    config = ''
      ;; banish ansi-term :)
      ;(defalias 'ansi-term (lambda (&rest _) (call-interactively #'vterm)))
      (setq vterm-shell "${pkgs.xonsh.pname}")
      (use-package vterm-toggle :demand t)
    '';
    systemDeps = with pkgs; [ cmake libtool ];
  };
}
