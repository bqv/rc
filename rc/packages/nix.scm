(define-module (rc packages nix)
  #:use-module (rc packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages web)
  #:use-module (rc packages mdbook)
  #:export (nixUnstable))

(define libcpuid
  (package
    (name "libcpuid")
    (version "0.5.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/anrieff/libcpuid.git")
               (commit (string-append "v" version))))
        (sha256
          (base64
            "m10LdtwBk1Lx31AJ4HixEYaCkT7EHpF9+tOV1rSA6VU="))
        (patches (search-patches "libcpuid-stdint.patch"))))
    (build-system gnu-build-system)
    (native-inputs
      `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("libtool" ,libtool)))
    (description "A small C library for x86 CPU detection and feature extraction")
    (synopsis "A small C library for x86 CPU detection and feature extraction")
    (home-page "http://libcpuid.sourceforge.net/")
    (license license:bsd-2)))

(define gtest
  (package
    (name "gtest")
    (version "1.10.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/google/googletest.git")
               (commit (string-append "release-" version))))
        (sha256
          (base32
            "1zbmab9295scgg4z2vclgfgjchfjailjnvzc6f5x9jvlsdi3dpwz"))))
    (build-system cmake-build-system)
    (description "Google's framework for writing C++ tests")
    (synopsis "Google's framework for writing C++ tests")
    (home-page "https://github.com/google/googletest")
    (license license:bsd-3)))

(define nlohmann_json
  (package
    (name "nlohmann_json")
    (version "3.9.2pre")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/nlohmann/json.git")
               (commit "e10a3fac8a255433146e3f06a703dc110fc3c3da")))
        (sha256
          (base32
            "11c494ibq5q9fi92qkq08d0a2q9qk8sa0hhnl6lcdb9pg7v0m19y"))))
    (build-system cmake-build-system)
    (arguments
      `(#:configure-flags (list "-DBuildTests=OFF"
                                "-DJSON_MultipleHeaders=ON")
        #:tests? #f))
    (description "Header only C++ library for the JSON file format")
    (synopsis "Header only C++ library for the JSON file format")
    (home-page "http://github.com/nlohmann/json")
    (license license:expat)))

(define lowdown
  (package
    (name "lowdown")
    (version "0.8.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://kristaps.bsd.lv/lowdown/snapshots/lowdown-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1f6lpyz5bbvwqadal8xcfjcm46zcycrgk5079isjyipg85zdyvb2"))
        (patches (search-patches "lowdown-shared.patch"))))
    (build-system gnu-build-system)
    (native-inputs
      `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("libtool" ,libtool)
        ("which" ,which)))
    (arguments
      `(#:configure-flags '()
        #:make-flags '("CFLAGS=-fPIC")
        #:phases (modify-phases
                   %standard-phases
                   (replace 'configure
                            (lambda* (#:key inputs outputs configure-flags #:allow-other-keys)
                                     ;; The configure script does not accept environment variables
                                     ;; as arguments.
                                     (let ((out (assoc-ref outputs "out")))
                                       (setenv "SHELL" (which "sh"))
                                       (setenv "CONFIG_SHELL" (which "sh"))
                                      ;(setenv "AUTOCONF" (string-append (assoc-ref inputs "autoconf")
                                      ;                                  "/bin/autoconf"))
                                       (apply invoke "./configure"
                                              (cons (string-append "PREFIX=" out)
                                                    configure-flags)))))
                   (replace 'check
                            (lambda _ (invoke "make" "regress"))))))
    (description "Simple markdown translator")
    (synopsis "Simple markdown translator")
    (home-page "https://kristaps.bsd.lv/lowdown/")
    (license license:isc)))

(define nixUnstable
  (package
    (inherit nix)
    (name "nix")
    (version "2.4pre")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/nixos/nix.git")
               (commit "5985b8b5275605ddd5e92e2f0a7a9f494ac6e35d")))
        (sha256
          (base64
            "2So7ZsD8QJlOXCYqdoj8naNgBw6O4Vw1MM2ORsaqlXc="))))
    (arguments
      (substitute-keyword-arguments (package-arguments nix)
        ((#:configure-flags configure-flags)
         `(cons*
            "--enable-gc"
            ,configure-flags))
        ((#:make-flags make-flags)
         `(cons*
            "CXXFLAGS=-Xc"
            ,make-flags))))
    (inputs
      (cons*
        `("gtest" ,gtest)
        `("libarchive" ,libarchive)
        `("libcpuid" ,libcpuid)
        `("nlohmann_json" ,nlohmann_json)
        `("zlib" ,zlib)
        (package-inputs nix)))
    (native-inputs
      (cons*
        `("autoconf" ,autoconf)
        `("autoconf-archive" ,autoconf-archive)
        `("automake" ,automake)
        `("bison" ,bison)
        `("flex" ,flex)
        `("gcc" ,gcc-10)
        `("jq" ,jq)
        `("lowdown" ,lowdown)
        `("rust-mdbook" ,rust-mdbook-0.4)
        (package-native-inputs nix)))))
