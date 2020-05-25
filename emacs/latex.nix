{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.latex = {
    package = lib.const null;
    mode = {
      "\\.tex\\'" = "latex-mode";
    };
    config = ''
      (add-hook 'doc-view-mode-hook 'auto-revert-mode)
      (add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)
      (add-hook 'LaTeX-mode-hook
                (lambda ()
                  (visual-line-mode t)
                  (turn-on-reftex))
                t)
      (setq pdf-latex-command "lualatex")
      (setq-default TeX-command-default "LaTeX"
                    TeX-PDF-mode t
                    Tex-engine 'lualatex)
    '';
  };
}
