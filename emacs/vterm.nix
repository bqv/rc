{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.vterm = {
    demand = true;
    package = epkgs: with epkgs; [ vterm emacs-libvterm ];
    config = ''
      ;; banish ansi-term :)
      ;(defalias 'ansi-term (lambda (&rest _) (call-interactively #'vterm)))
      (setq vterm-shell "${pkgs.xonsh.pname}")
    '';
    systemDeps = with pkgs; [ cmake libtool ];
  };
}
