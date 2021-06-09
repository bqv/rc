(define-module (rc home leaf)
               #:use-module (guix gexp)
               #:use-module (guix packages)
               #:use-module (guix utils)
               #:use-module (gnu services)
               #:use-module (gnu home)
               #:use-module (gnu home-services)
               #:use-module (gnu home-services gnupg)
               #:use-module (gnu home-services ssh)
               #:use-module (gnu home-services shells)
               #:use-module (gnu home-services files)
               #:use-module (rc home-services pipewire)
               #:use-module (gnu packages admin)
               #:use-module (gnu packages chromium)
               #:use-module (gnu packages dvtm)
               #:use-module (gnu packages emacs)
               #:use-module (gnu packages emacs-xyz)
               #:use-module (gnu packages irc)
               #:use-module (gnu packages linux)
               #:use-module (gnu packages messaging)
               #:use-module (gnu packages suckless)
               #:use-module (gnu packages terminals)
               #:use-module (gnu packages web-browsers)
               #:use-module (nongnu packages mozilla)
               #:use-module (flat packages emacs)
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
                   (bash-profile '("\
                                   export HISTFILE=$XDG_CACHE_HOME/.bash_history"))))

        (simple-service 'test-config
                        home-files-service-type
                        (list `("config/test.conf"
                                ,(plain-file "tmp-file.txt"
                                             "the content of ~/.config/test.conf"))))

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

        (list)))))
