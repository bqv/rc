(define-module (rc packages xmpppy)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix build-system python)
  #:export (python-xmpppy))

(define python-xmpppy
  (package
    (name "python-xmpppy")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "xmpppy" version))
        (sha256
          (base32
            "02cpd45a9k1nmmszr3hcjhs3lffdxz4r2rzm5idaxxgk578i062w"))))
    (build-system python-build-system)
    (home-page "http://xmpppy.sourceforge.net/")
    (synopsis "XMPP implementation in Python")
    (description "XMPP implementation in Python")
    (license gpl3)))
