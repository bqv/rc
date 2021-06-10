(define-module (rc home leaf)
               #:use-module (guix gexp)
               #:use-module (guix packages)
               #:use-module (guix utils)
               #:use-module (gnu system)
               #:use-module (gnu services)
               #:use-module (gnu home)
               #:use-module (gnu home-services)
               #:use-module (gnu home-services files)
               #:use-module (gnu home-services gnupg)
               #:use-module (gnu home-services shells)
               #:use-module (gnu home-services shepherd)
               #:use-module (gnu home-services ssh)
               #:use-module (rc home-services pipewire)
               #:use-module (gnu packages admin)
               #:use-module (gnu packages chromium)
               #:use-module (gnu packages dvtm)
               #:use-module (gnu packages emacs)
               #:use-module (gnu packages emacs-xyz)
               #:use-module (gnu packages irc)
               #:use-module (gnu packages linux)
               #:use-module (gnu packages messaging)
               #:use-module (gnu packages shells)
               #:use-module (gnu packages suckless)
               #:use-module (gnu packages terminals)
               #:use-module (gnu packages web-browsers)
               #:use-module (nongnu packages mozilla)
               #:use-module (flat packages emacs)
               #:use-module (rde packages)
               #:use-module (rc packages discord)
               #:use-module (rc packages pipewire-next)
               #:export (env))

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
      (home-directory "/home/leaf")
     ;(symlink-name ".guix-home")
      (packages (list firefox ungoogled-chromium nyxt
                      weechat irssi discord
                      dino profanity poezio gajim-full gajim-omemo gajim-openpgp
                      termite alacritty st dvtm-custom
                      emacs-pgtk-native-comp emacs-evil emacs-ivy emacs-vterm emacs-geiser))
      (services
        (cons*
          (service home-bash-service-type
                   (home-bash-configuration
                     (guix-defaults? #t)
                     (bash-profile '("export HISTFILE=$XDG_CACHE_HOME/.bash_history"))))
  
          (service home-fish-service-type
                   (home-fish-configuration
                     (package fish)
                     (config (list "export HISTFILE=$XDG_CACHE_HOME/.fish_history"
                                   #~(string-append "set fish_function_path $fish_function_path "
                                                    #$fish-foreign-env
                                                    "/share/fish/functions")
                                   "fenv source $HOME/.guix-home/setup-environment"
                                   "fenv $HOME/.guix-home/on-first-login"
                                   "fenv source $HOME/.guix-profile/etc/profile"))
                     (environment-variables
                       `(("VISUAL" . "nvim")
                         ("EDITOR" . "nvim")
                         ("NIX_PATH" . "nixpkgs=/nix/var/nix/profiles/system/flake/input/master")
                         ("GUIX" . "$HOME/.config/guix/current/share/guile/site/3.0")))
                     (aliases '(("vim" . "nvim")))
                     (abbreviations '())))
  
          (simple-service 'pipewire-add-asoundrd
                          home-files-service-type
                          (list `("config/alsa/asoundrc"
                                  ,(mixed-text-file
                                     "asoundrc"
                                     #~(string-append
                                         "<"
                                         #$(file-append
                                             pipewire-next "/share/alsa/alsa.conf.d/50-pipewire.conf")
                                         ">\n<"
                                         #$(file-append
                                             pipewire-next "/share/alsa/alsa.conf.d/99-pipewire-default.conf")
                                         ">\n"
                                         "
pcm_type.pipewire {
  lib " #$(file-append pipewire-next
                       "/lib/alsa-lib/libasound_module_pcm_pipewire.so") "
}
ctl_type.pipewire {
  lib " #$(file-append pipewire-next
                       "/lib/alsa-lib/libasound_module_ctl_pipewire.so") "
}
")))))

          (simple-service 'dbus-set-env
                          home-environment-variables-service-type
                          '(("DBUS_SESSION_BUS_ADDRESS"
                             . "unix:path=$XDG_RUNTIME_DIR/dbus.sock")))
                            ;; ("RTC_USE_PIPEWIRE" . "true")

          (simple-service 'dbus-shepherd-daemon
                          home-shepherd-service-type
                          (list
                            (shepherd-service
                              (provision '(dbus))
                              (start #~(make-forkexec-constructor
                                         (list #$(file-append (@@ (gnu packages glib) dbus)
                                                              "/bin/dbus-daemon")
                                               "--session"
                                               (string-append
                                                 "--address="
                                                 "unix:path="
                                                 (getenv "XDG_RUNTIME_DIR")
                                                 "/dbus.sock")))))))

          (simple-service 'pipewire-add-packages
                          home-profile-service-type
                          (append
                            ;; TODO: Should be in feature-sway
                            (list xdg-desktop-portal-latest
                                  xdg-desktop-portal-wlr-latest)
                            (list pipewire-next)))
  
         ;(service home-ssh-service-type
         ;         (home-ssh-configuration
         ;           (extra-config
         ;             (list
         ;               (ssh-include "config.*")
         ;               (ssh-host "savannah"
         ;                         '((compression . #f)))))))
  
          (service home-gnupg-service-type
                   (home-gnupg-configuration
                     (gpg-agent-config
                       (home-gpg-agent-configuration
                         (ssh-agent? #t)))))
  
          (service pipewire-service-type
                   (pipewire-configuration
                     (package pipewire-next)
                     (config (plain-file "pipewire.conf" ""))))
          (service pipewire-pulse-service-type
                   (pipewire-pulse-configuration
                     (package pipewire-next)
                     (config (plain-file "pipewire-pulse.conf" ""))))
          (service pipewire-media-session-service-type
                   (pipewire-media-session-configuration
                     (package pipewire-next)))
  
          (list))))))
