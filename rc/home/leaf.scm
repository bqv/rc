(define-module (rc home leaf)
               #:use-module (srfi srfi-1)
               #:use-module (guix gexp)
               #:use-module (guix monads)
               #:use-module (guix packages)
               #:use-module (guix profiles)
               #:use-module (guix store)
               #:use-module (guix utils)
               #:use-module (gnu system)
               #:use-module (gnu services)
               #:use-module (gnu services shepherd)
               #:use-module (gnu home)
               #:use-module (rc home)
               #:use-module (gnu home-services)
               #:use-module (gnu home-services files)
               #:use-module (gnu home-services gnupg)
               #:use-module (gnu home-services shells)
               #:use-module (gnu home-services shellutils)
               #:use-module (gnu home-services shepherd)
               #:use-module (gnu home-services ssh)
               #:use-module (gnu home-services wm)
               #:use-module (rc home-services pipewire)
               #:use-module (gnu packages abduco)
               #:use-module (gnu packages admin)
               #:use-module (gnu packages android)
               #:use-module (gnu packages chromium)
               #:use-module (gnu packages dvtm)
               #:use-module (gnu packages emacs)
               #:use-module (gnu packages emacs-xyz)
               #:use-module (gnu packages freedesktop)
               #:use-module (gnu packages irc)
               #:use-module (gnu packages linux)
               #:use-module (gnu packages messaging)
               #:use-module (gnu packages ncurses)
               #:use-module (gnu packages package-management)
               #:use-module (gnu packages pulseaudio)
               #:use-module (gnu packages python)
               #:use-module (gnu packages shells)
               #:use-module (gnu packages shellutils)
               #:use-module (gnu packages suckless)
               #:use-module (gnu packages task-management)
               #:use-module (gnu packages terminals)
               #:use-module (gnu packages web-browsers)
               #:use-module (gnu packages wm)
               #:use-module (gnu packages xdisorg)
               #:use-module (nongnu packages mozilla)
               #:use-module (nongnu packages steam-client)
               #:use-module (flat packages emacs)
               #:use-module (rde packages)
               #:use-module (rc packages discord)
               #:use-module (rc packages minecraft)
               #:use-module (rc packages pipewire)
               #:use-module ((rc packages zsh) #:prefix zsh-)
               #:export (env))

(define abduco-custom
  (package
    (inherit abduco)
    (arguments
      (substitute-keyword-arguments (package-arguments abduco)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-before 'build 'set-modifier
              (lambda _
                (substitute* "config.def.h"
                  (("CTRL\\('\\\\\\\\'\\)") "CTRL('^')"))
                (substitute* "abduco.1"
                  (("\\^\\\\\\\\") "^^"))
                #t))
            (add-after 'install 'store-config
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out")))
                  (mkdir-p (string-append out "/include/abduco"))
                  (copy-file "config.h" (string-append out "/include/abduco/config.h")))
                #t))))))))

(define dvtm-custom
  (package
    (inherit dvtm)
    (arguments
      (substitute-keyword-arguments (package-arguments dvtm)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-before 'build 'set-modifier
              (lambda _
                (substitute* "config.def.h"
                  (("CTRL\\('g'\\)") "CTRL('q')"))
                (substitute* "dvtm.1"
                  (("\\^g") "^q"))
                #t))
            (add-after 'install 'store-config
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out")))
                  (mkdir-p (string-append out "/include/dvtm"))
                  (copy-file "config.h" (string-append out "/include/dvtm/config.h")))
                #t))))))))

(define gajim-full
  (package
    (inherit gajim)
    (propagated-inputs (cons*
                         `("gajim-omemo" ,gajim-omemo)
                         `("gajim-openpgp" ,gajim-openpgp)
                         (package-propagated-inputs gajim)))))

(define (env os)
  (let* ((system (os)))
    (home-environment
     ;(symlink-name ".guix-home")
      (packages (list nyxt ungoogled-chromium (delayed 'firefox) (delayed 'emacs)
                      weechat irssi discord
                      dino profanity poezio gajim-full gajim-omemo gajim-openpgp
                      ncurses termite alacritty st dvtm-custom abduco-custom tmate
                      alsa-utils pavucontrol pulsemixer
                      taskwarrior mako adb fastboot
                      flatpak steam multimc))
      (services
        (cons*
          (service home-bash-service-type
                   (home-bash-configuration
                     (guix-defaults? #t)
                     (bash-profile
		       (list
                         "source $HOME/.profile"
			 "export HISTFILE=$XDG_CACHE_HOME/.bash_history"))))
  
          (service home-zsh-service-type
                   (home-zsh-configuration
                     (xdg-flavor? #f)
                     (package zsh)
                     (zshenv
                       (list
                         "source $HOME/.profile"
                         "HISTFILE=$HOME/.zhistory"
                         "HISTSIZE=1048576"
                         "SAVEHIST=65536"
                         "export VISUAL=nvim"
                         "export EDITOR=nvim"
                         "export ALTERNATE_EDITOR=emacsclient"
                         "export NIX_PATH=nixpkgs=/nix/var/nix/profiles/system/flake/input/master"
                         "export GUIX=$HOME/.config/guix/current/share/guile/site/3.0"))
                     (zprofile (list))
                     (zshrc
                       (list
                         #~(string-append "source " #$zsh-zplugin "/zplugin.zsh")
                         "autoload -Uz _zplugin"
                         "(( ${+_comps} )) && _comps[zplugin]=_zplugin"
			 "setopt promptsubst"
                         "setopt extended_history"       ; record timestamp of command in HISTFILE
                         "setopt hist_expire_dups_first" ; delete duplicates first when HISTFILE size exceeds HISTSIZE
                         "setopt hist_ignore_dups"       ; ignore duplicated commands history list
                         "setopt hist_ignore_space"      ; ignore commands that start with space
                         "setopt hist_verify"            ; show command with history expansion to user before running it
                         "setopt share_history"          ; share command history data

                         "zplugin light zsh-users/zsh-autosuggestions"
			 "zplugin ice compile\"*.lzui\" from\"notabug\"; zplugin load zdharma/zui"
			 "zplugin load zdharma/history-search-multi-word"
			;"zplugin ice as\"program\" make'!' atclone'./direnv hook zsh > zhook.zsh' atpull'%atclone' src\"zhook.zsh\"; zplugin light direnv/direnv"
			 "zplugin snippet OMZ::plugins/direnv/direnv.plugin.zsh"
			 "zplugin snippet OMZ::lib/git.zsh"
			 "zplugin ice wait\"0\" atload\"unalias grv\"; zplugin snippet OMZ::plugins/git/git.plugin.zsh"
			 "zplugin snippet OMZ::plugins/taskwarrior/taskwarrior.plugin.zsh"
                         "zplugin load momo-lab/zsh-abbrev-alias"
			 "zplugin light ptavares/zsh-z"
                        ;"zplugin load fabiogibson/envrc-zsh-plugin"
                         "zplugin snippet OMZ::themes/terminalparty.zsh-theme" ; nicoulaj
			;"zplugin ice as\"completion\"; zplugin snippet OMZ::plugins/adb/_adb"
			;"zplugin ice as\"completion\"; zplugin snippet OMZ::plugins/ipfs/_ipfs"
			;"zplugin ice as\"completion\"; zplugin snippet OMZ::plugins/ripgrep/_ripgrep"
                         "zplugin load zsh-users/zsh-completions"
			 "autoload -U compinit && compinit"
                         "zplugin ice wait\"0\" atinit\"zpcompinit; zpcdreplay\" lucid; zplugin load zsh-users/zsh-syntax-highlighting"
                         "zplugin ice wait\"0\" lucid; zplugin load zsh-users/zsh-history-substring-search"

                         "zle -N history-substring-search-up"
                         "zle -N history-substring-search-down"
                         "bindkey \"$terminfo[kcuu1]\" history-substring-search-up"
                         "bindkey \"$terminfo[kcud1]\" history-substring-search-down"
			 "zplugin snippet OMZ::lib/key-bindings.zsh"

			 "alias vim=nvim"
			 "abbrev-alias rg=\"rg -p\""
                         "abbrev-alias less=\"less -RF\""
                         "abbrev-alias jq=\"jq -C\""
                         "abbrev-alias ls\"exa\""
                         ))
                     (zlogin (list))
                     (zlogout (list))))
          (service home-zsh-autosuggestions-service-type)
  
          (simple-service 'add-imperative-profile
                          home-shell-profile-service-type
                          (list "GUIX_PROFILE=$HOME/.guix-profile"  
                                "source $GUIX_PROFILE/etc/profile"))

          (service home-fish-service-type
                   (home-fish-configuration
                     (package fish)
                     (config (list "export HISTFILE=$XDG_CACHE_HOME/.fish_history"
                                   #~(string-append "set fish_function_path $fish_function_path "
                                                    #$fish-foreign-env
                                                    "/share/fish/functions")
                                   "not set -q __fish_login_config_sourced\nand begin"
                                   "  fenv source $HOME/.profile"
                                   "end"))
                     (environment-variables
                       `(("VISUAL" . "nvim")
                         ("EDITOR" . "nvim")
                         ("NIX_PATH" . "nixpkgs=/nix/var/nix/profiles/system/flake/input/master")
                         ("GUIX" . "$HOME/.config/guix/current/share/guile/site/3.0")))
                     (aliases '(("vim" . "nvim")))
                     (abbreviations '(("rg" . "rg -p")
                                      ("less" . "less -RF")
                                      ("jq" . "jq -C")))))
  
          (simple-service 'pipewire-add-asoundrc
                          home-files-service-type
                          (list `("config/alsa/asoundrc"
                                  ,(mixed-text-file
                                     "asoundrc"
                                     #~(string-append
                                         "<"
                                         #$(file-append pipewire-0.3
                                                        "/share/alsa/alsa.conf.d/50-pipewire.conf")
                                         ">\n<"
                                         #$(file-append pipewire-0.3
                                                        "/share/alsa/alsa.conf.d/99-pipewire-default.conf")
                                         ">\n\npcm_type.pipewire {\nlib "
                                         #$(file-append pipewire-0.3
                                                        "/lib/alsa-lib/libasound_module_pcm_pipewire.so")
                                         "\n}\nctl_type.pipewire {\nlib "
                                         #$(file-append pipewire-0.3
                                                        "/lib/alsa-lib/libasound_module_ctl_pipewire.so")
                                         "\n}\n")
                                     ))))

          (simple-service 'dbus-set-env
                          home-environment-variables-service-type
                          '(("DBUS_SESSION_BUS_ADDRESS"
                             . "unix:path=$XDG_RUNTIME_DIR/dbus.sock")))
                            ;; ("RTC_USE_PIPEWIRE" . "true")

         ;(simple-service 'sway-set-env
         ;                home-environment-variables-service-type
         ;                '(("SWAYSOCK" . "$XDG_RUNTIME_DIR/dbus.sock")))

          (simple-service 'dbus-shepherd-daemon
                          home-shepherd-service-type
                          (list
                            (shepherd-service
                              (provision '(dbus))
                              (start #~(make-forkexec-constructor
                                         (list #$(file-append (@@ (gnu packages glib) dbus)
                                                              "/bin/dbus-daemon")
                                               "--nofork"
                                               "--session"
                                               (string-append
                                                 "--address="
                                                 "unix:path="
                                                 (or (getenv "XDG_RUNTIME_DIR")
                                                     (format #f "/run/user/~a" (getuid)))
                                                 "/dbus.sock")))))))

          (simple-service 'pipewire-add-packages
                          home-profile-service-type
                          (list xdg-desktop-portal
                                xdg-desktop-portal-wlr
                                pipewire-0.3))
  
          (service home-ssh-service-type
                   (home-ssh-configuration
                     (extra-config
                       (list
                         (ssh-host (host "ssh.dev.azure.com")
                                   (options
                                     `((identities-only . #t)
                                       (user . "git")
                                       (identity-file . "~/.ssh/id_rsa")
                                       (host-key-algorithms . ,(string-join '("ssh-rsa" "ssh-dss") ",")))))
                         (ssh-host (host "bitbucket.org")
                                   (options
                                     `((user . "git")
                                       (identity-file . "~/.ssh/id_rsa")
                                       (host-key-algorithms . ,(string-join '("ssh-rsa" "ssh-dss") ",")))))
                         (ssh-host (host "hub.darcs.net")
                                   (options
                                     `((user . "darcs")
                                       (identity-file . "~/.ssh/id_rsa")
                                       (host-key-algorithms . ,(string-join '("ssh-rsa" "ssh-dss") ",")))))
                         (ssh-host (host "10.0.0.3 delta")
                                   (options
                                     `()))
                         (ssh-host (host "dev.fron.io")
                                   (options
                                     `((port . 5022)
                                       (identity-file . "~/.ssh/id_rsa")
                                       (ciphers . ,(string-join '("aes128-ctr" "aes192-ctr" "aes256-ctr" "aes128-cbc" "3des-cbc") ","))
                                       (host-key-algorithms . ,(string-join '("ssh-rsa" "ssh-dss") ","))
                                       (kex-algorithms . "+diffie-hellman-group1-sha1"))))
                         (ssh-host (host "192.168.178.29 epsilon")
                                   (options
                                     `((user . "aion")
                                       (host-name . "192.168.178.29"))))
                         (ssh-host (host "github.com")
                                   (options
                                     `((user . "git")
                                       (identity-file . "~/.ssh/id_rsa")
                                       (host-key-algorithms . ,(string-join '("ssh-rsa" "ssh-dss") ",")))))
                         (ssh-host (host "192.168.178.42 leo")
                                   (options
                                     `((user . "kani")
                                       (host-name . "192.168.178.42"))))
                         (ssh-host (host "localhost")
                                   (options
                                     `((forward-agent . #t)
                                       (add-keys-to-agent . "ask"))))
                         (ssh-host (host "10.0.0.4 192.168.178.135 leo")
                                   (options
                                     `((user . "leaf")
                                       (host-name . "192.168.178.135"))))
                         (ssh-host (host "10.0.0.2 192.168.178.135 theta")
                                   (options
                                     `((user . "leaf")
                                       (host-name . "192.168.178.135"))))
                         (ssh-host (host "10.0.0.1 zeta.fron.io zeta")
                                   (options
                                     `((user . "bao")
                                       (host-name . "10.0.0.1")
                                       (identity-file . "~/.ssh/id_rsa")
                                       (set-env . "DVTM=off"))))))
                     (default-options
                       `((forward-agent . #f)
                         (compression . #f)
                         (server-alive-interval . 0)
                         (server-alive-count-max . 3)
                         (hash-known-hosts . #f)
                         (user-known-hosts-file . "/dev/null")
                         (control-master . #f)
                         (control-path . "~/.ssh/master-%r@%n:%p")
                         (control-persist . #f)
                         (verify-host-key-dns . #t)
                         (visual-host-key . #t)
                        ;(send-env . "LANG LC_*")
                        ;(ciphers . ,(string-join '("+aes128-cbc" "3des-cbc" "aes192-cbc") ","})
                         (ciphers . "+aes256-cbc")
                         (strict-host-key-checking . #f)))
                     (toplevel-options
                       `((,(string->symbol "\ninclude") . "config.*")))))
  
          (service home-gnupg-service-type
                   (home-gnupg-configuration
                     (gpg-agent-config
                       (home-gpg-agent-configuration
                         (ssh-agent? #t)))))
  
          (service pipewire-service-type
                   (pipewire-configuration
                     (package pipewire-0.3)
                     (config (plain-file "pipewire.conf" ""))))
          (service pipewire-pulse-service-type
                   (pipewire-pulse-configuration
                     (package pipewire-0.3)
                     (config (plain-file "pipewire-pulse.conf" ""))))
          (service pipewire-media-session-service-type
                   (pipewire-media-session-configuration
                     (package pipewire-0.3)))

          (service home-sway-service-type
                   (home-sway-configuration
                     (config
                       `((include "~/.config/sway/local")
                         (include ,(file-append sway "/etc/sway/config"))
                         (include ,(local-file "../../data/sway.config"))))))
          (simple-service 'sway-reload-config-on-change
                          home-run-on-change-service-type
                          `("files/config/sway/config"
                            ,#~(system* #$(file-append sway "/bin/swaymsg") "reload")))
          (simple-service 'packages-for-sway
                          home-profile-service-type
                          (list wofi qtwayland
                                xdg-desktop-portal xdg-desktop-portal-wlr))
          (simple-service 'set-wayland-specific-env-vars
                          home-environment-variables-service-type
                          ;; export NO_AT_BRIDGE=1
                          '(("XDG_CURRENT_DESKTOP" . "sway")
                            ("XDG_SESSION_TYPE" . "wayland")
                            ;; FIXME: Should be in feature-pipewire
                            ("RTC_USE_PIPEWIRE" . "true")
                            ("SDL_VIDEODRIVER" . "wayland")
                            ("MOZ_ENABLE_WAYLAND" . "1")
                            ("CLUTTER_BACKEND" . "wayland")
                            ("ELM_ENGINE" . "wayland_egl")
                            ("ECORE_EVAS_ENGINE" . "wayland-egl")
                            ("QT_QPA_PLATFORM" . "wayland-egl")
                            ("_JAVA_AWT_WM_NONREPARENTING" . "1")))
  
          (list))))))
