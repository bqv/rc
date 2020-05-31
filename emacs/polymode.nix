{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.polymode = {
    demand = true;
    config = ''
      (with-eval-after-load 'nix-mode
        (define-hostmode poly-nix-hostmode
          :mode 'nix-mode)

        (define-innermode poly-nix-dsquote-elisp-innermode
          :mode 'elisp-mode
          :head-matcher "''''\n"
          :tail-matcher "^\s*''''"
          :head-mode 'host
          :tail-mode 'host)

        (define-polymode nix-dsquoted-elisp-mode
          :hostmode 'poly-host/nix
          :innermodes '(poly-nix-dsquote-elisp-innermode)))
    '';
  };
}
