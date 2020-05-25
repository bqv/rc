{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.custom = {
    package = lib.const null;
    config = ''
      ;(if window-system
      ;  (load-theme 'hc-zenburn t)
      ;  (load-theme 'zenburn t))
      (progn
        (load-theme 'doom-nord t)
        (config-package doom-themes-ext-visual-bell
          :config
          (doom-themes-visual-bell-config))
        ;(all-the-icons-install-fonts "~/.emacs.d/fonts/")
        (config-package doom-themes-ext-neotree
          :config
          (doom-themes-neotree-config))
        (setq doom-themes-treemacs-theme "doom-colors")
        (config-package doom-themes-ext-treemacs
          :config
          (doom-themes-treemacs-config))
        (config-package doom-themes-ext-org
          :config
          (doom-themes-org-config))
        (doom-modeline-mode 1))
      (progn
        ;; How tall the mode-line should be. It's only respected in GUI.
        ;; If the actual char height is larger, it respects the actual height.
        (setq doom-modeline-height 25)

        ;; How wide the mode-line bar should be. It's only respected in GUI.
        (setq doom-modeline-bar-width 3)

        ;; How to detect the project root.
        ;; The default priority of detection is `ffip' > `projectile' > `project'.
        ;; nil means to use `default-directory'.
        ;; The project management packages have some issues on detecting project root.
        ;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
        ;; to hanle sub-projects.
        ;(setq doom-modeline-project-detection 'project)

        ;; Determines the style used by `doom-modeline-buffer-file-name'.
        ;;
        ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
        ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
        ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
        ;;   truncate-with-project => emacs/l/comint.el
        ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
        ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
        ;;   truncate-all => ~/P/F/e/l/comint.el
        ;;   relative-from-project => emacs/lisp/comint.el
        ;;   relative-to-project => lisp/comint.el
        ;;   file-name => comint.el
        ;;   buffer-name => comint.el<2> (uniquify buffer name)
        (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)

        ;; Whether display icons in mode-line. Respects `all-the-icons-color-icons'.
        (setq doom-modeline-icon (display-graphic-p))
        (defun bqv/modeline-icon (old-fn &rest args)
          (let ((doom-modeline-icon (display-graphic-p)))
            (apply old-fn args)))
        (advice-add #'doom-modeline-icon :around #'bqv/modeline-icon)

        ;; Whether display the icon for `major-mode'. Respects `doom-modeline-icon'.
        (setq doom-modeline-major-mode-icon t)

        ;; Whether display the colorful icon for `major-mode'.
        ;; Respects `doom-modeline-major-mode-icon'.
        (setq doom-modeline-major-mode-color-icon t)

        ;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
        (setq doom-modeline-buffer-state-icon t)

        ;; Whether display the modification icon for the buffer.
        ;; Respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
        (setq doom-modeline-buffer-modification-icon t)

        ;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
        (setq doom-modeline-unicode-fallback t)

        ;; Whether display the minor modes in mode-line.
        (setq doom-modeline-minor-modes (featurep 'minions))

        ;; If non-nil, a word count will be added to the selection-info modeline segment.
        (setq doom-modeline-enable-word-count t)

        ;; Major modes in which to display word count continuously.
        ;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
        (setq doom-modeline-continuous-word-count-modes '(text-mode))

        ;; Whether display the buffer encoding.
        (setq doom-modeline-buffer-encoding t)

        ;; Whether display the indentation information.
        (setq doom-modeline-indent-info t)

        ;; If non-nil, only display one number for checker information if applicable.
        (setq doom-modeline-checker-simple-format t)

        ;; The maximum number displayed for notifications.
        (setq doom-modeline-number-limit 99)

        ;; The maximum displayed length of the branch name of version control.
        (setq doom-modeline-vcs-max-length 12)

        ;; Whether display the perspective name. Non-nil to display in mode-line.
        (setq doom-modeline-persp-name t)

        ;; If non nil the default perspective name is displayed in the mode-line.
        (setq doom-modeline-display-default-persp-name nil)

        ;; Whether display the `lsp' state. Non-nil to display in mode-line.
        (setq doom-modeline-lsp t)

        ;; Whether display the GitHub notifications. It requires `ghub' package.
        (setq doom-modeline-github nil)

        ;; The interval of checking GitHub.
        (setq doom-modeline-github-interval (* 30 60))

        ;; Whether display the modal state icon.
        ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
        (setq doom-modeline-modal-icon t)

        ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
        (setq doom-modeline-mu4e t)

        ;; Whether display the IRC notifications. It requires `circe' or `erc' package.
        (setq doom-modeline-irc t)

        ;; Function to stylize the irc buffer names.
        (setq doom-modeline-irc-stylize 'identity)

        ;; Whether display the environment version.
        (setq doom-modeline-env-version t)
        ;; Or for individual languages
        (setq doom-modeline-env-enable-python t)
        (setq doom-modeline-env-enable-ruby t)
        (setq doom-modeline-env-enable-perl t)
        (setq doom-modeline-env-enable-go t)
        (setq doom-modeline-env-enable-elixir t)
        (setq doom-modeline-env-enable-rust t)

        ;; Change the executables to use for the language version string
        (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
        (setq doom-modeline-env-ruby-executable "ruby")
        (setq doom-modeline-env-perl-executable "perl")
        (setq doom-modeline-env-go-executable "go")
        (setq doom-modeline-env-elixir-executable "iex")
        (setq doom-modeline-env-rust-executable "rustc")

        ;; What to dispaly as the version while a new one is being loaded
        (setq doom-modeline-env-load-string "...")

        ;; Hooks that run before/after the modeline version string is updated
        (setq doom-modeline-before-update-env-hook nil)
        (setq doom-modeline-after-update-env-hook nil))
      (set-face-attribute 'menu nil :weight 'semi-bold)
      (setq display-time-string-forms
            '((if (and (not display-time-format) display-time-day-and-date)
                  (format-time-string "%a %b %e " now) "")
              (propertize
               (format-time-string (or display-time-format (if display-time-24hr-format "%H:%M" "%-I:%M%p")) now)
               (quote help-echo)
               (format-time-string "%a %b %e, %Y" now))
              (if (and (boundp 'mail) mail)
                (concat " "
                        (propertize
                         display-time-mail-string
                         (quote display)
                         (\` (when (and display-time-use-mail-icon (display-graphic-p)) (\,@ display-time-mail-icon) (\,@ (if (and display-time-mail-face (memq (plist-get (cdr display-time-mail-icon) :type) (quote (pbm xbm)))) (let ((bg (face-attribute display-time-mail-face :background))) (if (stringp bg) (list :background bg)))))))
                         (quote face)
                         display-time-mail-face
                         (quote help-echo)
                         "You have new mail; mouse-2: Read mail"
                         (quote mouse-face)
                         (quote mode-line-highlight)
                         (quote local-map)
                         (make-mode-line-mouse-map (quote mouse-2) read-mail-command)))
                "")))
      (display-time)
      (doom-modeline-def-segment misc-info
        "Mode line construct for miscellaneous information.
      By default, this shows the information specified by `global-mode-string'."
        (unless (doom-modeline--active)
            '("" mode-line-misc-info)))
    '';
  };
}
