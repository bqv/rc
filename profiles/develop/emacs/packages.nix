{ pkgs, ... }: {
  environment.systemPackages = let
    buildEmacsPackage = drv: pkgs.stdenv.mkDerivation (drv // {
      installPhase = ''
        mkdir -p $out/share/emacs/site-lisp
        cp *.el *.elc $out/share/emacs/site-lisp/
      '';
    });

    bitwarden = buildEmacsPackage rec {
      name = "emacs-bitwarden";
      src = pkgs.fetchFromGitHub {
        owner = "seanfarley"; repo = name;
        rev = "e03919ca68c32a8053ddea2ed05ecc5e454d8a43";
        sha256 = "1bbxh01856vg8acwrjbq3hyxa81zcvfby7k1j5hdfps918xy10m2";
      };
    };
    ivy-clipmenu = buildEmacsPackage rec {
      name = "ivy-clipmenu.el";
      src = pkgs.fetchFromGitHub {
        owner = "wpcarro"; repo = name;
        rev = "ef25acf3f058fe1ede3a29fae2e9cdac8b08cd17";
        sha256 = "1yzvaf95pncfi1r3xj8h6393dfvx291q3ahdwpp7qn3jh71kjx6k";
      };
    };
    ivy-exwm = buildEmacsPackage rec {
      name = "ivy-exwm";
      src = pkgs.fetchFromGitHub {
        owner = "pjones"; repo = name;
        rev = "32f107374aef01b9ae00f1647233d50b4ea659e0";
        sha256 = "1shs1zh8nr2lwxlvrhnhxxjn5g0p21vkjxnjgha1sn07pg7v3iqq";
      };
    };
    flycheck-purescript = buildEmacsPackage rec {
      name = "flycheck-purescript";
      src = pkgs.fetchFromGitHub {
        owner = "bsermons"; repo = name;
        rev = "a3f5e64fe56aedf9703540b4755a2e6e044cbe72";
        sha256 = "0qm048ypfzbrqd4a9ffn1ay3rhh58nacd9z78lph8mmj4ri1v2cc";
      };
    };
    eterm-256color = buildEmacsPackage rec {
      name = "eterm-256color";
      src = pkgs.fetchFromGitHub {
        owner = "dieggsy"; repo = name;
        rev = "0f0dab497239ebedbc9c4a48b3ec8cce4a47e980";
        sha256 = "00ins8n92p5aspr6bjrvn5y5w0ximakk22yklsfmkav4h10al4as";
      };
    };
  in with pkgs; [
    git w3m findutils fish zsh vim
    cmake gnumake gcc libtool libvterm gtk3 rls age #rust-analyzer
  ] ++ (with emacsPackages; [
    # usr-init
    use-package auto-compile gcmh diminish epkg
    # usr-main
    #server
    # usr-crit-bufmgmt.el
    persp-mode neotree persp-projectile window-purpose ivy-purpose
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
    # usr-lang-kotlin.el
    kotlin-mode flycheck-kotlin
    # usr-lang-android.el
    android-mode android-env
    # usr-tool-flymake.el
    flymake
    # usr-tool-avy.el
    avy ace-window switch-window
    # usr-tool-vcs.el
    magit git-gutter git-timemachine projectile
    # usr-tool-nix.el
    nix-buffer nix-mode nix-update direnv
    # usr-tool-media.el
    emms emms-player-simple-mpv
    # usr-util-org.el
    org calfw calfw-org
    # usr-util-web.el
    w3m
    # usr-util-irc.el
    tracking weechat
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
    vterm emacs-libvterm fish-completion xterm-color eterm-256color
    # rest
  ]);
}
