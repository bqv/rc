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
      (setq org-clock-idle-time 15)

      ;; Show lot of clocking history so it's easy to pick items off the `C-c I` list
      (setq org-clock-history-length 23)

      (defun bqv/org-clock-in ()
        (interactive)
        (org-clock-in '(4)))

      (global-set-key (kbd "C-c I") #'bqv/org-clock-in)
      (global-set-key (kbd "C-c O") #'org-clock-out)

      ;; Resume clocking task when emacs is restarted
      (org-clock-persistence-insinuate)
      ;; Save the running clock and all clock history when exiting Emacs, load it on startup
      (setq org-clock-persist t)
      ;; Resume clocking task on clock-in if the clock is open
      (setq org-clock-in-resume t)
    '';
  };
}
