{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.polymode = {
    demand = true;
    config = ''
      (with-eval-after-load 'nix-mode
        (define-hostmode poly-nix-hostmode
          :mode 'nix-mode)

        (define-innermode poly-nix-dsquote-elisp-innermode
          :mode 'lisp-mode
          :head-matcher "''''\n"
          :tail-matcher "^\s*''''"
          :head-mode 'host
          :tail-mode 'host)

        (define-polymode nix-dsquoted-elisp-mode
          :hostmode 'poly-nix-hostmode
          :innermodes '(poly-nix-dsquote-elisp-innermode)))
    '';
  };
}

## Local Variables: ***
## mode: nix-dsquoted-elisp
## End: ***
