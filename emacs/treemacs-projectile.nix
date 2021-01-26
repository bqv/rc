{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.treemacs-projectile = {
    demand = true;
    after = [ "treemacs" "projectile" ];
    config = ''
      (defun bqv/treemacs-open ()
        (unless (equalp (treemacs-current-visibility) 'visible)
          (with-selected-window (selected-window)
            (treemacs))))
      (add-hook 'projectile-after-switch-project-hook 'bqv/treemacs-open)
    '';
  };
}
