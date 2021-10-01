(define-module (rc packages emacs weechat) #:use-module (guix build-system emacs) #:use-module (guix download) #:use-module (guix packages) #:use-module (gnu packages emacs) #:use-module (gnu packages emacs-xyz) #:export ())
(define-public emacs-weechat
  (package
    (name "emacs-weechat")
    (version "20190520.1551")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/weechat-"
               version
               ".tar"))
        (sha256
          (base32
            "1s4140kgghz95mxksaqayxxmm8n9z06sq72xnq1sd50rhmm5l5wx"))))
    (build-system emacs-build-system)
    (propagated-inputs
      `(("emacs-s" ,emacs-s)
        ("emacs-tracking" ,emacs-tracking)))
    (home-page
      "https://github.com/the-kenny/weechat.el")
    (synopsis
      "Chat via WeeChat's relay protocol in Emacs")
    (description
      "This package provides a way to chat via WeeChat's relay protocol in
Emacs.

Please see README.org on how to use it.
")
    (license #f)))


emacs-weechat
