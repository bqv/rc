{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    git fish
    w3m findutils
    cmake gnumake gcc libtool libvterm gtk3 rls age rust-analyzer
  ] ++ (with emacsPackagesFor config.programs.emacs.package.unwrapped; [
    use-package auto-compile gcmh diminish epkg log4e

    # usr-crit-bufmgmt.el
    persp-mode neotree persp-projectile window-purpose ivy-purpose treemacs treemacs-evil treemacs-magit treemacs-persp treemacs-projectile lsp-treemacs treemacs-icons-dired
    # usr-crit-evil.el
    evil
    # usr-crit-syntax.el
    idle-highlight-mode flycheck company company-box company-lsp lsp-mode lsp-ui yasnippet
    # usr-crit-theme.el
    solarized-theme zenburn-theme hc-zenburn-theme material-theme doom-themes doom-modeline #palette misc-cmds
    # usr-crit-wm.el
    exwm desktop-environment buffer-move exwm-edit pinentry all-the-icons dashboard ivy-exwm ivy-clipmenu #exwm-config map cl exwm-input exwm-manage exwm-randr exwm-systemtray exwm-workspace
    # usr-crit-completion.el
    smex ivy fzf counsel ivy-rich counsel-projectile swiper ivy-hydra which-key
    # usr-lang-cpp.el
    meson-mode
    execline
    # usr-lang-racket.el
    racket-mode
    # usr-lang-rust.el
    toml-mode rust-mode cargo flycheck-rust
    # usr-lang-latex.el
    auctex-lua auctex-latexmk latex-preview-pane latex-pretty-symbols latex-extra elsa flycheck-elsa #latex auctexdoc-view
    # usr-lang-haskell.el
    yaml-mode haskell-mode company-cabal flycheck-haskell
    # usr-lang-purescript.el
    purescript-mode flycheck-purescript
    # usr-lang-typescript.el
    typescript-mode flycheck-jest
    # usr-lang-kotlin.el
    kotlin-mode flycheck-kotlin
    # usr-lang-android.el
    android-mode android-env
    # usr-tool-flymake.el
    flymake
    # usr-tool-avy.el
    avy ace-window switch-window
    # usr-tool-vcs.el
    magit forge git-gutter git-timemachine projectile #magithub
    # usr-tool-nix.el
    nix-buffer nix-mode nix-update direnv
    # usr-tool-media.el
    emms emms-player-simple-mpv
    # usr-util-org.el
    org calfw calfw-org
    # usr-util-web.el
    w3m
    # usr-util-irc.el
    tracking weechat-patched
    # usr-util-bitwarden.el
    bitwarden
    # usr-util-auth.el
    pass oauth2 #auth-source
    # usr-util-systemd.el
    daemons
    # usr-util-mail.el
    mew gnus-desktop-notify nnreddit nnhackernews pkgs.mu wanderlust
    # usr-util-mastodon.el
    mastodon
    # usr-util-games.el
    steam
    # usr-util-shell.el
    vterm emacs-libvterm fish-completion xterm-color eterm-256color multi-vterm xonsh-mode
    # rest
  ]);
}
