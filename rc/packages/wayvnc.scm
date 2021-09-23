(define-module (rc packages wayvnc)
  #:use-module (rc packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages python)
  #:use-module (gnu packages man)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:export (wayvnc))

(define aml
  (package
    (name "aml")
    (version "0.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/any1/aml.git")
               (commit "1d8185ec15c68074cec1fd252c01c5ecad877b73")))
        (sha256
          (base32
            "0mxmzlhiv88hm4sf8kyawyrml8qy1xis019hdyb5skl9g95z9yyf"))))
    (build-system meson-build-system)
    (native-inputs `())
    (inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/any1/aml")
    (synopsis "Another main loop")
    (description "Another main loop")
    (license license:isc)))

(define libneatvnc
  (package
    (name "libneatvnc")
    (version "0.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/any1/neatvnc.git")
               (commit "b1d32694d0a310e36da1cf84420c827bbf665755")))
        (sha256
          (base32
            "1wpq1vyjqra877vwc3n4i0c1dyhmabyn993cslf1k142ikyc0a8w"))))
    (build-system meson-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("pixman" ,pixman)
              ("aml" ,aml)
              ("zlib" ,zlib)
              ("libdrm" ,libdrm)
              ("libjpeg-turbo" ,libjpeg-turbo) ;?
              ("gnutls" ,gnutls) ;?
              ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/any1/aml")
    (synopsis "A VNC server library")
    (description "
      This is a liberally licensed VNC server library that's intended to be
      fast and neat. Goals:
      - Speed
      - Clean interface
      - Interoperability with the Freedesktop.org ecosystem
    ")
    (license license:isc)))

(define wayvnc
  (package
    (name "wayvnc")
    (version "0.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/any1/wayvnc.git")
               (commit "13323a742f1449355852b2501b25d8df804cefee")))
        (sha256
          (base32
            "0q48fgh6gf3jicy4bk3kq18h9lhqfq9qz32ri6j9ffvbb8mcw64s"))))
    (build-system meson-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("scdoc" ,scdoc)
                     ("wayland" ,wayland)))
    (inputs `(("pixman" ,pixman)
              ("libdrm" ,libdrm)
              ("libxkbcommon" ,libxkbcommon)
              ("wayland" ,wayland)
              ("aml" ,aml)
              ("libneatvnc" ,libneatvnc)
              ("zlib" ,zlib)
              ("libjpeg-turbo" ,libjpeg-turbo)
              ("gnutls" ,gnutls)
              ("libx11" ,libx11)
              ("linux-pam" ,linux-pam)
              ("python" ,python)
              ("python-wrapper" ,python-wrapper)))
    (home-page "https://github.com/any1/wayvnc")
    (synopsis "A VNC server for wlroots based Wayland compositors")
    (description "
      This is a VNC server for wlroots based Wayland compositors. It attaches
      to a running Wayland session, creates virtual input devices and exposes a
      single display via the RFB protocol. The Wayland session may be a
      headless one, so it is also possible to run wayvnc without a physical
      display attached.
    ")
    (license license:isc)))

wayvnc
