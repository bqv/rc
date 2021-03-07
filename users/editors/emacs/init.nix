{ config, pkgs, lib, usr, ... }:

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
    (require 'cl-lib)
    (require 's)
    (defun update-load-paths ()
      (cl-flet ((add-paths-for (dir) (let ((default-directory dir))
                                       (when (file-directory-p default-directory)
                                         (normal-top-level-add-subdirs-to-load-path)))))
        (add-paths-for "/run/current-system/sw/share/emacs/site-lisp")
        (add-paths-for (format "/etc/profiles/per-user/%s/share/emacs/site-lisp" (getenv "USER")))
        (add-paths-for (format "%s/.nix-profile/share/emacs/site-lisp" (getenv "HOME"))))
      (setq load-path (delete-dups
                        (append (seq-filter (lambda (p) (s-contains? "/eln-" p)) load-path)
                                load-path nil)))) (update-load-paths)
    (setq package-enable-at-startup nil)
    (setq load-prefer-newer t)
  '';

  # Setup base packaging - leaf
  package-init = ''
    (defvar pdmp/dumping-p nil
      "non-nil when creating a dump file.")

    (defmacro pdmp/if-dumping (then &rest else)
      "Evaluate IF if running with a dump file, else evaluate ELSE."
      (declare (indent 1))
      `(if pdmp/dumping-p
           ,then
         ,@else))

    (defvar pdmp/dumped-p nil
      "non-nil when a dump file is loaded.
    (Because dump.el sets this variable).")

    (defmacro pdmp/if-dumped (then &rest else)
      "Evaluate IF if running with a dump file, else evaluate ELSE."
      (declare (indent 1))
      `(if pdmp/dumped-p
           ,then
         ,@else))

    (require 'leaf)
    (leaf leaf-keywords
      :ensure t
      :init
      ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
      (leaf hydra :ensure t)
      (leaf el-get :ensure t)
      (leaf blackout :ensure t)

      :setq
      (leaf-defaults . '(:require t))
      :config
      ;; initialize leaf-keywords.el
      (leaf-keywords-init))
    (leaf auto-compile
      :leaf-defer nil
      :config
      (auto-compile-on-load-mode)
      (auto-compile-on-save-mode)
      (setq auto-compile-display-buffer               nil)
      (setq auto-compile-mode-line-counter            t)
      (setq auto-compile-source-recreate-deletes-dest t)
      (setq auto-compile-toggle-deletes-nonlib-dest   t)
      (setq auto-compile-update-autoloads             t))
    (leaf gcmh
      :config
      (gcmh-mode t))
    (leaf diminish
      :leaf-defer nil)
    (leaf epkg)
    (require 'log4e)
    (log4e:deflogger "log" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                     (error . "error")
                                                     (warn  . "warn")
                                                     (info  . "info")
                                                     (debug . "debug")
                                                     (trace . "trace")))
    (log--log-enable-logging)
    (log--log-enable-messaging)

    (defun load-ffi-systemd ()
      (require 'ffi)
      (define-ffi-library lib/systemd "${pkgs.systemd}/lib/libsystemd.so")
      (define-ffi-function lib/systemd/sd_notify "sd_notify" :int [:int :pointer] lib/systemd)
      (defun sd_notify (&rest assocs)
        (let* ((assignments (mapcar (lambda (p) (format "%s=%s" (car p) (cdr p))) assocs))
               (lstr (s-join "\n" assignments)))
          (with-ffi-string (cstr lstr)
            (lib/systemd/sd_notify 0 cstr))))

      (defun watchdog-systemd-notify ()
        (with-timeout (10)
          (sd_notify '("READY" . 1)
                     );;(`("WATCHDOG_USEC" . ,(* 120 1000000)))
          (log--trace "Notified at %s" (format-time-string "%D %T"))))
      ;(run-at-time t 10 #'watchdog-systemd-notify)
      t)
    (defun config-end ()
      (message
        "Configuring...done (%.3fs) [after-init]"
        (float-time (time-subtract (current-time)
                                   before-user-init-time)))
      (fmakunbound 'config-end))

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
      "Load a package PACKAGE by calling leaf, with ARGS."
      (let ((name (cond ((symbolp package) package)
                        ((stringp package) (make-symbol package))
                        (t nil))))
        (if noninteractive
            `(require ',name)
          `(config-segment ',name
                           (leaf ,name ,@args)))))

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

    (when (eq window-system 'pgtk)
      (pgtk-use-im-context t))
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
    (save-place-mode 1)
    (savehist-mode 1)
    (setq history-length 65536)
    (global-display-line-numbers-mode 1)
    (global-so-long-mode 1)
    (size-indication-mode 1)
    (setq scroll-margin 0
          scroll-conservatively 100000
          scroll-preserve-screen-position 1)
    (setq-default indent-tabs-mode nil)
    (setq view-read-only t)
    (setq compilation-scroll-output t)
    (setq max-mini-window-height 0.5)
    (when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings))
    (when (and (not noninteractive) ; breaks in batch mode
               (fboundp 'winner-mode))
      (winner-mode 1))
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
            kept-old-versions 2    ; and some old ones, too
            create-lockfiles nil)) ; no .#blah files
    (global-auto-revert-mode t)
    (add-hook 'before-save-hook 'whitespace-cleanup)
    (global-set-key (kbd "C-x k") 'kill-this-buffer)
    (setq confirm-kill-emacs 'y-or-n-p)

    ;; long lines hack
    (setq-default bidi-display-reordering nil)
    (setq bidi-inherit-bpa t)

    (setq default-input-method "programmer-dvorak")
    (defun activate-default-input-method ()
      (activate-input-method default-input-method))
    ;; new buffers
    (defadvice switch-to-buffer (after activate-input-method activate)
      (activate-default-input-method))
    ;; minibuffer
    ;(add-hook 'minibuffer-setup-hook #'activate-default-input-method)
    ;; scratch
    (with-current-buffer "*scratch*"
      (activate-default-input-method))
    ;; toggleback
    (advice-add #'toggle-input-method :after #'toggle-input-method-back)
    (defvar toggle-input-method-timeout 60 "Input method switchback timeout")
    (defun toggle-input-method-back (&rest args)
      (run-at-time toggle-input-method-timeout nil
                   `(lambda ()
                      (ignore-errors (with-current-buffer ,(current-buffer)
                                       (activate-default-input-method))))))

    (config-package custom
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
      :leaf-defer nil
      :config
      (setq server-auth-dir "~/.emacs.d/server/"
          );server-use-tcp t
           ;server-host "0.0.0.0"
           ;server-port 2222)
      (if (not (server-running-p))
          (server-start)
       ));(shell-command
         ;  (format "cp --reflink=always %s/server %s/%s" server-auth-dir server-auth-dir
         ;    (format "server@%d" (round (time-to-seconds))))
         ;  nil)))

    (progn
      ${lib.concatMapStrings ({ sym, script }: ''
        (config-segment '${sym}
          ${script emacs.pkgs})
      '') (lib.mapAttrsToList (sym: cfg@{ script, ... }: {
        inherit sym script;
      }) config.emacs.loader)}
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
    secrets = usr.secrets.emacs.user;
  in ''
    ${startup-pre}

    ${package-init}

    ${startup}
  '';
}

## Local Variables: ***
## mode: nix-dsquoted-emacslisp ***
## End: ***
