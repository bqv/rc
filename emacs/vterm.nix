{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.vterm = {
    demand = true;
    config = ''
      ;; banish ansi-term :)
      ;(defalias 'ansi-term (lambda (&rest _) (call-interactively #'vterm)))
      (setq vterm-shell "${pkgs.xonsh.pname}")
    '';
    systemDeps = with pkgs; [ cmake libtool libvterm ];
  };
}
