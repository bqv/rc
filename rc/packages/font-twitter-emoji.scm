(define-module (rc packages font-twitter-emoji)
  #:use-module (rc packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system font)
  #:use-module (gnu packages)
  #:export (font-twitter-emoji))

(define font-twitter-emoji
  (package
    (name "font-twitter-emoji")
    (version "13.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/eosrei/twemoji-color-font/releases/download/v"
                            version
                            "/TwitterColorEmoji-SVGinOT-Linux-"
                            version
                            ".tar.gz"))
        (sha256 (base32
                  "1mn2cb6a3v0q8i81s9a8bk49nbwxq91n6ki7827i7rhjkncb0mbn"))))
    (build-system font-build-system)
    (arguments
      `(#:phases (modify-phases %standard-phases
                  (add-after 'install 'install-fontconfig
                    (lambda* (#:key source outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (mkdir-p (string-append out "/etc/fonts/conf.d/"))
                        (copy-file "fontconfig/56-twemoji-color.conf"
                                   (string-append out "/etc/fonts/conf.d/56-twemoji-color.conf"))))))))
    (home-page "https://github.com/eosrei/twemoji-color-font")
    (synopsis "Twemoji color font")
    (description "Color emoji SVGinOT font using Twitter Unicode 10 emoji with diversity and country flags")
    (license license:cc-by4.0)))
