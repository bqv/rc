(use-modules (guix packages)
             (guix download)
             (guix licenses)
             (guix build-system python))

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
  (license gpl3))
