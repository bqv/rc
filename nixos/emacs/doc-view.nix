{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.doc-view = {
    package = lib.const null;
    config = ''
      (add-hook 'doc-view-mode-hook
                (lambda ()
                  (define-key doc-view-mode-map [wheel-down] 'doc-view-next-line-or-next-page)
                  (define-key doc-view-mode-map [double-wheel-down]
                              (lambda ()
                                (interactive)
                                (doc-view-next-line-or-next-page 2)))
                  (define-key doc-view-mode-map [triple-wheel-down]
                              (lambda ()
                                (interactive)
                                (doc-view-next-line-or-next-page 3)))
                  (define-key doc-view-mode-map [wheel-up] 'doc-view-previous-line-or-previous-page)
                  (define-key doc-view-mode-map [double-wheel-up]
                              (lambda ()
                                (interactive)
                                (doc-view-previous-line-or-previous-page 2)))
                  (define-key doc-view-mode-map [triple-wheel-up]
                              (lambda ()
                                (interactive)
                                (doc-view-previous-line-or-previous-page 3))))
                t)
    '';
  };
}
