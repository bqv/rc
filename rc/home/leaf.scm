(define-module (rc home leaf)
               #:use-module (guix gexp)
               #:use-module (guix packages)
               #:use-module (gnu home)
               #:use-module (gnu home-services)
               #:use-module (gnu home-services gnupg)
               #:use-module (gnu home-services ssh)
               #:use-module (gnu home-services shells)
               #:use-module (gnu home-services files)
               #:use-module (gnu services)
               #:use-module (gnu packages admin)
               #:use-module (gnu packages chromium)
               #:use-module (gnu packages emacs)
               #:use-module (gnu packages emacs-xyz)
               #:use-module (gnu packages irc)
               #:use-module (gnu packages messaging)
               #:use-module (gnu packages terminals)
               #:use-module (gnu packages web-browsers)
               #:use-module (nongnu packages mozilla)
               #:use-module (flat packages emacs)
               #:use-module (rc packages discord)
               #:export (env))

(define gajim-full
  (package
    (inherit gajim)
    (propagated-inputs (cons*
                         `("gajim-omemo" ,gajim-omemo)
                         `("gajim-openpgp" ,gajim-openpgp)
                         (package-propagated-inputs gajim)))))

(define (env)
  (home-environment
    (home-directory "/home/leaf")
   ;(symlink-name ".guix-home")
    (packages (list firefox ungoogled-chromium nyxt
                    dino weechat irssi profanity poezio gajim-full gajim-omemo gajim-openpgp discord
                    termite alacritty
                    emacs-pgtk-native-comp emacs-evil emacs-ivy emacs-vterm emacs-geiser))
    (services
      (list
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
        ))))
