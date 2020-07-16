{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.dashboard = {
    demand = true;
    config = ''
      (dashboard-setup-startup-hook)
      (setq dashboard-center-content t)
      (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
      (setq dashboard-items '(;(recents  . 5)
                              (bookmarks . 5)
                              (projects . 5)
                              (agenda . 5)))
      (defun config-goto-error (package)
        (with-current-buffer (find-file user-init-file)
          (goto-char (point-min)) (search-forward package) (recenter)))
      (defun dashboard-init-errors (list-size)
        (dashboard-insert-section "Config Errors:"
                                  (config-errors)
                                  list-size
                                  "e"
                                  `(lambda (&rest ignore) (config-goto-error (car ',el)))
                                  (concat (car el) " :: " (cadr el))))
      (add-to-list 'dashboard-item-generators  '(errors . dashboard-init-errors))
      (add-to-list 'dashboard-items '(errors) t)
    '';
  };
}
