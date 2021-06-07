(define-module (rc packages biboumi)
  #:use-module (rc packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages base)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages sqlite)
  #:export (biboumi))

(define catch
  (package
    (name "biboumi")
    (version "2.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://lab.louiz.org/louiz/Catch.git")
               (commit "0a34cc201ef28bf25c88b0062f331369596cb7b7")))
        (sha256
          (base32
            "0ad0sjhmzx61a763d2ali4vkj8aa1sbknnldks7xlf4gy83jfrbl"))))
    (build-system trivial-build-system)
    (home-page "")
    (synopsis "")
    (description "")
    (license license:boost1.0)))

(define biboumi
  (package
    (name "biboumi")
    (version "9.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://git.louiz.org/biboumi/snapshot/biboumi-"
                            version
                            ".tar.xz"))
        (sha256
          (base32
            "1jvygri165aknmvlinx3jb8cclny6cxdykjf8dp0a3l3228rmzqy"))
        (patches (search-patches "biboumi-without-catch.patch"))
        (modules '((guix build utils)))
        (snippet
          #~(begin
              (mkdir-p "tests")
              (copy-file #$(file-append (package-source catch)
                                        "/single_include/catch.hpp")
                         "./tests/catch.hpp")
              #t))))
    (build-system cmake-build-system)
    (arguments
      `(#:phases
        (modify-phases %standard-phases
                       (add-before 'configure 'fix-cmakelists
                                   (lambda* (#:key outputs #:allow-other-keys)
                                     (let* ((out (assoc-ref outputs "out")))
                                       (substitute* "CMakeLists.txt"
                                                    (("/etc/biboumi")
                                                     (string-append out "/etc/biboumi")))))))
        #:tests? #f))
    (native-inputs `(("python-sphinx" ,python-sphinx)
                     ("python" ,python-wrapper)))
    (inputs `(("util-linux" ,util-linux "lib") ; libuuid
              ("libgcrypt" ,libgcrypt)
              ("expat" ,expat)
              ("botan" ,botan)
              ("libiconv" ,libiconv)
              ("libidn" ,libidn)
              ("postgresql" ,postgresql)
              ("sqlite" ,sqlite)
              ("pkg-config" ,pkg-config)))
    ; systemd udns
    (home-page "https://biboumi.louiz.org/")
    (synopsis "XMPP gateway to IRC")
    (description "XMPP gateway to IRC")
    (license license:zlib)))
