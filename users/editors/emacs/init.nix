{ config, lib, usr, ... }:

let
  emacs = config.programs.emacs.package;

  # Basic init - no packaging available
  startup-pre = ''
    (defvar before-user-init-time (current-time)
      "Value of `current-time' when Emacs begins loading `current-file'.")

    (message "Loading Emacs...done (%.3fs)"
             (float-time (time-subtract before-user-init-time
                                        before-init-time)))

    (setq current-file (or load-file-name (buffer-file-name)))
    (setq current-path (file-name-directory current-file))

    (message "Initializing...")
    (defun update-load-paths ()
      (let ((default-directory "/run/current-system/sw/share/emacs/site-lisp"))
        (when (file-directory-p default-directory)
          (normal-top-level-add-subdirs-to-load-path)))
      (let ((default-directory (format "/etc/profiles/per-user/%s/share/emacs/site-lisp" (getenv "USER"))))
        (when (file-directory-p default-directory)
          (normal-top-level-add-subdirs-to-load-path)))
      (let ((default-directory (format "%s/.nix-profile/share/emacs/site-lisp" (getenv "HOME"))))
        (when (file-directory-p default-directory)
          (normal-top-level-add-subdirs-to-load-path)))
      ) (update-load-paths)
    (setq package-enable-at-startup nil)
    (setq load-prefer-newer t)
  '';

  # Setup base packaging - use-package
  package-init = ''
    (setq use-package-verbose t)
    (require 'use-package)
    (use-package auto-compile
      :demand t
      :config
      (auto-compile-on-load-mode)
      (auto-compile-on-save-mode)
      (setq auto-compile-display-buffer               nil)
      (setq auto-compile-mode-line-counter            t)
      (setq auto-compile-source-recreate-deletes-dest t)
      (setq auto-compile-toggle-deletes-nonlib-dest   t)
      (setq auto-compile-update-autoloads             t))
    (use-package gcmh
      :config
      (gcmh-mode t))
    (use-package diminish
      :demand t)
    (use-package epkg)
    (use-package log4e
      :config
      (log4e:deflogger "log" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                       (error . "error")
                                                       (warn  . "warn")
                                                       (info  . "info")
                                                       (debug . "debug")
                                                       (trace . "trace")))
      (log--log-enable-logging)
      (log--log-enable-messaging))

    (setq init-done nil)
    (defun config-end ()
      (message
        "Configuring...done (%.3fs) [after-init]"
        (float-time (time-subtract (current-time)
                                   before-user-init-time)))
      (fmakunbound 'config-end)
      (setq init-done t));(setq debug-on-error t))

    (defvar config-registry '()
      "Profiling and initialization status data")

    (defconst config-scope '()
      "The current config breadcrumb trail.")

    (defmacro config-segment (name &rest body)
      "Run a timed config operation BODY with name NAME."
      `(progn
        (let* ((start-time (current-time))
               (error-state nil)
               (config-scope (cons (symbol-name ,name) config-scope))
               (config-name (string-join (reverse config-scope) "::")))
          (condition-case the-problem
            (progn ,@body)
            (error (setq error-state the-problem))
            (warn (setq error-state the-problem)))
          (if error-state
            (let* ((end-time (current-time))
                   (delta (time-subtract end-time start-time))
                   (rethrow (lambda () (funcall #'signal
                                                (car error-state)
                                                (cdr error-state))))
                   (registry-data (record 'config-err
                                           config-scope
                                           `(,start-time . ,end-time)
                                           error-state)))
              (log--fatal (concat config-name " failed after %.3fs")
                       (float-time delta))
              (add-to-list 'config-registry `(,config-name . ,registry-data))
              (if noninteractive (funcall rethrow)))
            (let* ((end-time (current-time))
                   (delta (time-subtract end-time start-time))
                   (registry-data (record 'config-ok
                                           config-scope
                                           `(,start-time . ,end-time)
                                           delta)))
              (log--info (concat config-name " finished in %.3fs")
                       (float-time delta))
              (add-to-list 'config-registry `(,config-name . ,registry-data)))))))

    (defmacro config-package (package &rest args)
      "Load a package PACKAGE by calling use-package, with ARGS."
      (let ((name (cond ((symbolp package) package)
                        ((stringp package) (make-symbol package))
                        (t nil))))
        `(config-segment ',name
                         (use-package ,name ,@args))))

    (defun config-errors ()
      (let ((error-registry (seq-remove (lambda (c) (eq (type-of (cdr c)) 'config-ok)) config-registry)))
        (mapcar (lambda (c)
                  `(,(car c) ,@(cdr (aref (cdr c) 3))))
                error-registry)))
  '';

  # Load everything else
  startup = ''
    (log--info "Bootstrapping...done (%.3fs)"
             (float-time (time-subtract (current-time)
                                        before-user-init-time)))

    (setq inhibit-startup-message t)
    (setq inhibit-startup-buffer-menu t)
    (setq inhibit-startup-screen t)
    (setq inhibit-startup-echo-area-message "locutus")
    (setq initial-buffer-choice t)
    (setq initial-scratch-message "")
    (scroll-bar-mode 0)
    (tool-bar-mode 0)
    (menu-bar-mode 1)
    (fringe-mode 1) ; pixels
    (blink-cursor-mode 0)
    (show-paren-mode 1)
    (setq show-paren-delay 0)
    ;(global-hl-line-mode 1)
    (column-number-mode 1)
    (line-number-mode 1)
    (savehist-mode 1)
    (setq history-length 65536)
    (global-display-line-numbers-mode 1)
    (size-indication-mode 1)
    (setq scroll-margin 0
          scroll-conservatively 100000
          scroll-preserve-screen-position 1)
    (setq-default indent-tabs-mode nil)
    (setq compilation-scroll-output t)
    (when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings))
    (fset 'yes-or-no-p 'y-or-n-p)
    (progn
      ;; Put backup files neatly away
      (let ((backup-dir (expand-file-name "backups" user-emacs-directory))
            (auto-saves-dir (expand-file-name "autosaves" user-emacs-directory)))
        (dolist (dir (list backup-dir auto-saves-dir))
          (when (not (file-directory-p dir))
            (make-directory dir t)))
        (setq backup-directory-alist `(("." . ,backup-dir))
              auto-save-file-name-transforms `((".*" ,(concat auto-saves-dir "/") t))
              auto-save-list-file-prefix (concat auto-saves-dir "/" ".saves-")
              tramp-backup-directory-alist `((".*" . ,backup-dir))
              tramp-auto-save-directory auto-saves-dir))
      (setq backup-by-copying t    ; Don't delink hardlinks
            delete-old-versions t  ; Clean up the backups
            version-control t      ; Use version numbers on backups,
            kept-new-versions 5    ; keep some new versions
            kept-old-versions 2))  ; and some old ones, too
    (global-auto-revert-mode t)
    (add-hook 'before-save-hook 'whitespace-cleanup)
    (global-set-key (kbd "C-x k") 'kill-this-buffer)
    (setq confirm-kill-emacs 'y-or-n-p)

    (config-package custom
      :no-require t
      :config
      (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
      (when (file-exists-p custom-file)
        (load custom-file)))

    (cl-flet ((config-custom-arg (switch)
      (let ((found-switch (member switch command-line-args)))
        (setq command-line-args (delete switch command-line-args))
        found-switch)))
             (let ((short-arg-new (config-custom-arg "-n"))
                   (long-arg-new (config-custom-arg "--new"))
                   (long-arg-daemon (config-custom-arg "--daemon"))
                   (long-arg-test (config-custom-arg "--test")))
               (setq config-arg-new (or long-arg-new short-arg-new))
               (setq config-arg-daemon long-arg-daemon)
               (setq config-arg-test long-arg-test)))

    (config-package server
      :demand t
      :config
      (if (not (server-running-p))
          (server-start)))

    (progn
      ${lib.concatMapStrings ({ sym, script }: ''
        (config-segment '${sym}
          ${script emacs.pkgs})
      '') (lib.mapAttrsToList (sym: cfg@{ script, ... }: {
        inherit sym script;
      }) config.emacs-loader)}
      (config-end))

    (log--info "Loading...done (%.3fs)"
             (float-time (time-subtract (current-time)
                                        before-user-init-time)))
    (makunbound 'before-user-init-time)
  '';
in usr.elisp.writeFile {
  name = "init";
  description = "Initialization script";
  text = let
    secrets = import ../../../secrets/emacs.user.nix;
  in ''
    ${startup-pre}

    ${package-init}

    ${startup}
  '';
}
