{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.org = {
    demand = true;
    config = ''
      (global-set-key "\C-cl" 'org-store-link)
      (global-set-key "\C-ca" 'org-agenda)
      (global-set-key "\C-cc" 'org-capture)
      (global-set-key "\C-cb" 'org-switchb)
      (setq bookmark-save-flag t)
      (setq org-log-done 'time)
      (setq org-log-done 'note)
     ;(setq org-directory "~/var/org/"
     ;      org-agenda-files (list org-directory))
      (defmacro .org (name &rest ignored)
        `,(expand-file-name (concat name ".org") org-directory))
    '';
  };
}
