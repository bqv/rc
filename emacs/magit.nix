{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.magit = {
    demand = true;
    bind = {
      "C-x g" = "magit-status";
      "C-x M-g" = "magit-dispatch-popup";
      "C-c g" = "magit-dispatch";
    };
    config = ''
      (setq magit-log-show-refname-after-summary t)
      (setq magit-clone-default-directory (expand-file-name "~/dev/"))
      (setq ediff-window-setup-function 'ediff-setup-windows-plain)
      (setq magit-section-initial-visibility-alist nil)
      (setq magit-wip-merge-branch t)
      (magit-wip-mode)

      ; temp fix
      (defun server-switch-buffer--with-editor-server-window-alist
          (fn &optional next-buffer &rest args)
        "Honor `with-editor-server-window-alist' (which see)."
        (let ((server-window (with-current-buffer
                                 (or next-buffer (current-buffer))
                               (when with-editor-mode
                                 (setq with-editor-previous-winconf
                                       (current-window-configuration)))
                               (with-editor-server-window))))
          (apply fn next-buffer args)))
    '';
    systemDeps = with pkgs; [ git ];
  };
}
