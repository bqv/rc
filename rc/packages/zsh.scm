(define-module (rc packages zsh)
               #:use-module (guix git-download)
               #:use-module (guix utils)
               #:use-module (guix packages)
               #:use-module (guix build-system copy)
               #:use-module (gnu packages shells)
               #:export (antigen
                         zplugin))

(define antigen
  (package
    (name "zsh-antigen")
    (version "2.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/zsh-users/antigen")
                     (commit (string-append "v" version))))
              (sha256
                (base32
                  "1hqnwdskdmaiyi1p63gg66hbxi1igxib6ql8db3w950kjs1cs7rq"))))
    (inputs `(("zsh" ,zsh)))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("bin/antigen.zsh" "share/zsh/antigen.zsh"))))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

(define zplugin
  (package
    (name "zsh-zplugin")
    (version "2.07")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/sbilly/zplugin")
                     (commit (string-append "v" version))))
              (sha256
                (base32
                  "17h58c5fbs9h1jz2xk9sximkk674kj4d6vm1hispf4akwq5ia0pc"))))
    (inputs `(("zsh" ,zsh)))
    (build-system copy-build-system)
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

zplugin
