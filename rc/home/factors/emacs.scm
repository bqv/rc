(define-module (rc home factors emacs)
  #:use-module (rc home)
  #:use-module (rc utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (flat packages emacs)
  #:export (use-emacs-services))

(define (use-emacs-services services)
  (cons*
    (service home-emacs-service-type
             (home-emacs-configuration
               (package emacs-pgtk-native-comp)
              ;(package (inferior-package->package (delayed 'emacs) #:license #f))
               (elisp-packages (list
                                 emacs-ace-window ; ace-window.nix
                                ;emacs-ahg ; ahg.nix
                                 emacs-all-the-icons ; all-the-icons.nix
                                ;emacs-android-env ; android-env.nix
                                ;emacs-android-mode ; android-mode.nix
                                ;emacs-aria2 ; aria2.nix
                                ;emacs-auctex-latexmk ; auctex-latexmk.nix
                                ;emacs-auctex-lua ; auctex-lua.nix
                                 emacs-auctex ; auctex.nix
                                ;emacs-auth-source ; auth-source.nix
                                 emacs-avy ; avy.nix
                                ;emacs-bitwarden ; bitwarden.nix
                                ;emacs-bookmark ; bookmark.nix
                                 emacs-buffer-move ; buffer-move.nix
                                ;emacs-bufler ; bufler.nix
                                ;emacs-calfw-org ; calfw-org.nix
                                 emacs-calfw ; calfw.nix
                                ;emacs-cargo ; cargo.nix
                                ;emacs-cl ; cl.nix
                                 emacs-company-box ; company-box.nix
                                 emacs-company-cabal ; company-cabal.nix
                                ;emacs-company-terraform ; company-terraform.nix
                                 emacs-company ; company.nix
                                 emacs-counsel-projectile ; counsel-projectile.nix
                                 emacs-counsel ; counsel.nix
                      ;buildfail;emacs-csharp-mode ; csharp-mode.nix
                                ;emacs-csproj-mode ; csproj-mode.nix
                                ;emacs-custom ; custom.nix
                                 emacs-daemons ; daemons.nix
                                 emacs-dante ; dante.nix
                                 emacs-dap-mode ; dap-mode.nix
                                ;emacs-darcsum ; darcsum.nix
                                 emacs-dashboard ; dashboard.nix
                                 emacs-desktop-environment ; desktop-environment.nix
                                 emacs-dired-du ; dired-du.nix
                                 emacs-direnv ; direnv.nix
                                 emacs-disk-usage ; disk-usage.nix
                                ;emacs-doc-view ; doc-view.nix
                                 emacs-doom-modeline ; doom-modeline.nix
                                 emacs-doom-themes ; doom-themes.nix
                                ;emacs-dotnet ; dotnet.nix
                                 emacs-eglot ; eglot.nix
                                ;emacs-el-get ; el-get.nix
                                ;emacs-elsa ; elsa.nix
                                ;emacs-emacs-ffi ; emacs-ffi.nix
                                ;emacs-emacsbridge ; emacsbridge.nix
                                 emacs-emms-mode-line-cycle ; emms-mode-line-cycle.nix
                                 emacs-emms ; emms.nix
                                 emacs-envrc ; envrc.nix
                                ;emacs-erc ; erc.nix
                                ;emacs-eshell-git-prompt ; eshell-git-prompt.nix
                                 emacs-eshell-z ; eshell-z.nix
                                ;emacs-eterm-256color ; eterm-256color.nix
                                 emacs-evil-collection ; evil-collection.nix
                                 emacs-evil ; evil.nix
                                 emacs-execline ; execline.nix
                                 emacs-explain-pause-mode ; explain-pause-mode.nix
                                ;emacs-exwm-config ; exwm-config.nix
                                 emacs-exwm-edit ; exwm-edit.nix
                                ;emacs-exwm-input ; exwm-input.nix
                                ;emacs-exwm-manage ; exwm-manage.nix
                                ;emacs-exwm-randr ; exwm-randr.nix
                                ;emacs-exwm-systemtray ; exwm-systemtray.nix
                                ;emacs-exwm-workspace ; exwm-workspace.nix
                                ;emacs-exwm-xim ; exwm-xim.nix
                                 emacs-exwm ; exwm.nix
                                 emacs-fish-completion ; fish-completion.nix
                                ;emacs-fix-input ; fix-input.nix
                                ;emacs-flycheck-elsa ; flycheck-elsa.nix
                                 emacs-flycheck-haskell ; flycheck-haskell.nix
                                ;emacs-flycheck-jest ; flycheck-jest.nix
                                ;emacs-flycheck-kotlin ; flycheck-kotlin.nix
                                ;emacs-flycheck-pos-tip ; flycheck-pos-tip.nix
                                ;emacs-flycheck-posframe ; flycheck-posframe.nix
                                ;emacs-flycheck-purescript ; flycheck-purescript.nix
                                 emacs-flycheck-rust ; flycheck-rust.nix
                                 emacs-flycheck ; flycheck.nix
                                ;emacs-flymake ; flymake.nix
                                 emacs-forge ; forge.nix
                                ;emacs-fsharp-mode ; fsharp-mode.nix
                                ;emacs-fzf ; fzf.nix
                                 emacs-geiser
                                 emacs-geiser-guile
                                 emacs-general ; general.nix
                                ;emacs-gh-notify ; gh-notify.nix
                                 emacs-git-gutter ; git-gutter.nix
                                 emacs-git-timemachine ; git-timemachine.nix
                                ;emacs-gnus ; gnus.nix
                                 emacs-go-mode ; go-mode.nix
                                 emacs-guix
                                 emacs-haskell-mode ; haskell-mode.nix
                                ;emacs-hc-zenburn-theme ; hc-zenburn-theme.nix
                                 emacs-helm ; helm.nix
                                 emacs-helpful ; helpful.nix
                                 emacs-hy-mode ; hy-mode.nix
                                ;emacs-idle-highlight-mode ; idle-highlight-mode.nix
                                 emacs-ivy-clipmenu ; ivy-clipmenu.nix
                                 emacs-ivy-hydra ; ivy-hydra.nix
                                ;emacs-ivy-purpose ; ivy-purpose.nix
                                 emacs-ivy-rich ; ivy-rich.nix
                                 emacs-ivy ; ivy.nix
                                ;emacs-jabber ; jabber.nix
                                 emacs-json-mode ; json-mode.nix
                                 emacs-kotlin-mode ; kotlin-mode.nix
                                ;emacs-latex-extra ; latex-extra.nix
                                ;emacs-latex-pretty-symbols ; latex-pretty-symbols.nix
                                ;emacs-latex-preview-pane ; latex-preview-pane.nix
                                ;emacs-latex ; latex.nix
                                 emacs-leaf
                                ;emacs-leaf-convert ; leaf-convert.nix
                                ;emacs-leaf-keywords ; leaf-keywords.nix
                                 emacs-log4e ; log4e.nix
                                ;emacs-lsp-haskell ; lsp-haskell.nix
                                 emacs-lsp-ivy ; lsp-ivy.nix
                                 emacs-lsp-mode ; lsp-mode.nix
                                 emacs-lsp-treemacs ; lsp-treemacs.nix
                                 emacs-lsp-ui ; lsp-ui.nix
                                ;emacs-lui ; lui.nix
                                 emacs-magit ; magit.nix
                                 emacs-map ; map.nix
                                 emacs-mastodon ; mastodon.nix
                                ;emacs-material-theme ; material-theme.nix
                                 emacs-matrix-client ; matrix-client.nix
                      ;buildfail;emacs-md4rd ; md4rd.nix
                                 emacs-meson-mode ; meson-mode.nix
                                ;emacs-minimap ; minimap.nix
                                ;emacs-multi-vterm ; multi-vterm.nix
                                ;emacs-nix-buffer ; nix-buffer.nix
                                 emacs-nix-mode ; nix-mode.nix
                                ;emacs-nix-update ; nix-update.nix
                                 emacs-nnreddit ; nnreddit.nix
                                 emacs-oauth2 ; oauth2.nix
                                ;emacs-omnisharp ; omnisharp.nix
                                 emacs-org-jira ; org-jira.nix
                                 emacs-org ; org.nix
                      ;collision;emacs-pinentry ; pinentry.nix
                                 emacs-polymode ; polymode.nix
                                ;emacs-popwin ; popwin.nix
                                 emacs-projectile ; projectile.nix
                                 emacs-purescript-mode ; purescript-mode.nix
                                 emacs-racket-mode ; racket-mode.nix
                                ;emacs-rcirc-color ; rcirc-color.nix
                                ;emacs-rcirc ; rcirc.nix
                                 emacs-restclient ; restclient.nix
                                 emacs-reverse-im ; reverse-im.nix
                                 emacs-rust-mode ; rust-mode.nix
                                 emacs-s ; s.nix
                                 ; scraps+bufmgmt.el
                                 ; scraps+misc.el
                                 ; scraps+shell.el
                                 emacs-shackle ; shackle.nix
                                 emacs-slime-company ; slime-company.nix
                                 emacs-slime ; slime.nix
                                ;emacs-sln-mode ; sln-mode.nix
                                 emacs-sly ; sly.nix
                                 emacs-smex ; smex.nix
                                 emacs-solarized-theme ; solarized-theme.nix
                                ;emacs-sql ; sql.nix
                                ;emacs-steam ; steam.nix
                                ;emacs-straight ; straight.nix
                                 emacs-swiper ; swiper.nix
                                ;emacs-terraform-doc ; terraform-doc.nix
                                 emacs-terraform-mode ; terraform-mode.nix
                                ;emacs-tex ; tex.nix
                                 emacs-toml-mode ; toml-mode.nix
                                 emacs-tracking ; tracking.nix
                                 emacs-tramp ; tramp.nix
                                 emacs-transient ; transient.nix
                                ;emacs-treemacs-evil ; treemacs-evil.nix
                                ;emacs-treemacs-icons-dired ; treemacs-icons-dired.nix
                                ;emacs-treemacs-magit ; treemacs-magit.nix
                                ;emacs-treemacs-projectile ; treemacs-projectile.nix
                                 emacs-treemacs ; treemacs.nix
                                 emacs-typescript-mode ; typescript-mode.nix
                                 emacs-undo-tree ; undo-tree.nix
                                ;emacs-vc-darcs ; vc-darcs.nix
                                 emacs-vterm-toggle ; vterm-toggle.nix
                                 emacs-vterm ; vterm.nix
                                 emacs-w3m ; w3m.nix
                                 emacs-web-mode ; web-mode.nix
                                ;emacs-webkit ; webkit.nix
                                 emacs-webpaste ; webpaste.nix
                                ;emacs-weechat ; weechat.nix
                                 emacs-which-key ; which-key.nix
                                ;emacs-whitespace ; whitespace.nix
                                 emacs-window-purpose ; window-purpose.nix
                                ;emacs-xonsh-mode ; xonsh-mode.nix
                                 emacs-xterm-color ; xterm-color.nix
                                 emacs-yaml-mode ; yaml-mode.nix
                                 emacs-yasnippet ; yasnippet.nix
                                 emacs-zenburn-theme ; zenburn-theme.nix
                                 emacs-modus-themes
                                 ))
               (server-mode? #t)
               (xdg-flavor? #f)
               (init-el
                 `((setq custom-file "~/.emacs.d/custom.el")
                   (when (file-exists-p custom-file)
                     (load custom-file t))
                   ,#~""
                   (setq inhibit-splash-screen t)
                   ,#~""
                   ;; (define-key global-map (kbd "M-/") 'hippie-expand)

                   (tool-bar-mode 0)
                   (evil-mode t)
                   (require 'vterm)
                   (require 'windmove)
                   (windmove-default-keybindings)
                   (column-number-mode 1)
                   (save-place-mode 1)
                   (show-paren-mode 1)

                   (setq-default indent-tabs-mode nil)
                   (setq save-interprogram-paste-before-kill t)
                   (setq mouse-yank-at-point t)
                   (setq require-final-newline t)
                   (add-hook 'prog-mode-hook
                             (lambda () (setq show-trailing-whitespace t)))

                   (load-theme 'modus-vivendi t)

                   (defvar bootstrap-version)
                   (let ((bootstrap-file
                           (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
                         (bootstrap-version 5))
                     (unless (file-exists-p bootstrap-file)
                       (with-current-buffer
                         (url-retrieve-synchronously
                           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                           'silent 'inhibit-cookies)
                         (goto-char (point-max))
                         (eval-print-last-sexp)))
                     (load bootstrap-file nil 'nomessage))
                   (straight-use-package 'use-package)

                   (use-package modular-config
                     :straight t
                     :custom
                     (modular-config-list '(
                                           ;(minimal (core appearance))
                                           ;(mail (core vi mail appearance helm space emoji not-org mail gnus begin-mail))
                                           ;(news (begin-news core vi helm space web appearance helm core-post not-org))
                                           ;;; core web org emoji 
                                           ;(tracking (core vi space appearance org begin-tracking not-org))
                                           ;(programming (core appearance ivy org programming vc))
                                           ;(org (core vi space begin-org appearance completion files web finance helm vc programming custom server auto language-server shell help projects subtitles dashboard core-post org afterload wakatime music modeline))
                                           ;(chat (core appearance space vi irc slack begin-chat))
                                           ;(orgtest (org))
                                           ;(wm (wm))
                                           ;(doom (org doom))
                                           ;(utilities (core))
                                           ;(main (core appearance programming emoji ivy web org finance news mail documents server space workspace dashboard core-post))
                                            (none ())))
                     (modular-config-default 'none)
                     (modular-config-path "~/.emacs.d/lisp")
                     :config
                     (modular-config-command-line-args-process))))
              ;(early-init-el
              ;  `(,(slurp-file-gexp (local-file "~/.emacs.d/early-init.el"))))
               ;;; TODO: Rebuilding packages with emacs will be useful for
               ;;; native-comp, but for some reason dash.el fails to build,
               ;;; need to investigate the issue.
               (rebuild-elisp-packages? #f)
               ))
    services))
