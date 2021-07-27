(define-module (rc packages gajim)
               #:use-module (srfi srfi-1)
               #:use-module (guix gexp)
               #:use-module (guix git-download)
               #:use-module (guix download)
               #:use-module (guix monads)
               #:use-module (guix packages)
               #:use-module (guix profiles)
               #:use-module (guix store)
               #:use-module (guix utils)
               #:use-module (guix build-system trivial)
               #:use-module (guix build-system python)
               #:use-module (gnu packages)
               #:use-module (gnu packages gnome)
               #:use-module (gnu packages gnupg)
               #:use-module (gnu packages gstreamer)
               #:use-module (gnu packages gtk)
               #:use-module (gnu packages messaging)
               #:use-module (gnu packages python)
               #:use-module (gnu packages python-crypto)
               #:use-module (gnu packages python-xyz)
               #:use-module ((guix licenses) #:prefix license:)
               #:export (gajim-full))

(define-public gajim-omemo
  (package
    (name "gajim-omemo")
    (version "2.8.6")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri
        (string-append
         "https://ftp.gajim.org/plugins_releases/omemo_"
         version ".zip"))
       (sha256
        (base32 "0pc1sa9wbhmkcljr4jzny98gkspx6jwg3lv3820p6mb3hvbcv5cp"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (share (in-vicinity out "share/gajim/plugins"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p share)
           (copy-recursively source share)
           #t))))
    (propagated-inputs
     `(("python-axolotl" ,python-axolotl)
       ("python-axolotl-curve25519" ,python-axolotl-curve25519)
       ("python-cryptography" ,python-cryptography)
       ("python-qrcode" ,python-qrcode)))
    (synopsis "Gajim OMEMO plugin")
    (description "Gajim-OMEMO is a plugin that adds support for the OMEMO
Encryption to Gajim.  OMEMO is an XMPP Extension Protocol (XEP) for secure
multi-client end-to-end encryption.")
    (home-page
     "https://dev.gajim.org/gajim/gajim-plugins/-/wikis/OmemoGajimPlugin")
    (license license:gpl3+)))

(define-public gajim-openpgp
  (package
    (name "gajim-openpgp")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri
        (string-append
         "https://ftp.gajim.org/plugins_releases/openpgp_"
         version ".zip"))
       (sha256
        (base32 "0xck7xyw536278nmzjzd9lyv90gs8alrazmf1i26fwn3scpl7q7b"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (share (in-vicinity out "share/gajim/plugins"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p share)
           (copy-recursively source share)
           #t))))
    (propagated-inputs
     `(("python-cryptography" ,python-cryptography)
       ("python-gnupg" ,python-gnupg)
       ("python-gpg" ,python-gpg)))
    (synopsis "Gajim OpenPGP plugin")
    (description "Gajim-OpenPGP is a plugin that adds support for the OpenPGP
Encryption to Gajim.")
    (home-page "https://dev.gajim.org/gajim/gajim-plugins/-/wikis/OpenPGPplugin")
    (license license:gpl3+)))

(define-public gajim-pgp
  (package
    (name "gajim-pgp")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri
        (string-append
         "https://ftp.gajim.org/plugins_releases/pgp_"
         version ".zip"))
       (sha256
        (base32 "1smhg0ca3h69i6hcsynz976m56as7izspiv7hr8fkabpbgyr3n9b"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (share (in-vicinity out "share/gajim/plugins"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p share)
           (copy-recursively source share)
           #t))))
    (propagated-inputs
     `(("python-cryptography" ,python-cryptography)
       ("python-gnupg" ,python-gnupg)
       ("python-gpg" ,python-gpg)))
    (synopsis "Gajim PGP plugin")
    (description "Gajim-PGP is a plugin that adds support for the PGP
Encryption to Gajim.")
    (home-page "https://dev.gajim.org/gajim/gajim-plugins/-/wikis/PGPplugin")
    (license license:gpl3+)))

(define-public gajim-plugin-installer
  (package
    (name "gajim-plugin-installer")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri
        (string-append
         "https://ftp.gajim.org/plugins_releases/plugin_installer_"
         version ".zip"))
       (sha256
        (base32 "1mcqpvklypihkhcs5cy62hq5b2dpwcg4pv2073gh2yblr4ir41v7"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (share (in-vicinity out "share/gajim/plugins"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p share)
           (copy-recursively source share)
           #t))))
    (synopsis "Gajim plugin installer plugin")
    (description "Gajim plugin installer is a plugin that adds support for plugins to Gajim.")
    (home-page "https://dev.gajim.org/gajim/gajim-plugins/-/wikis/plugininstallerplugin")
    (license license:gpl3+)))

(define gajim-full
  (package
    (inherit gajim)
    (version "1.4.0-dev1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://dev.gajim.org/gajim/gajim.git")
              (commit "318ca493a6d716bb69d927c0bbb6d9b543c6ab35")))
       (sha256
        (base32 "0wxrg1smdi3gvxq99jq6fdmw6qy49gxc8av9zw07wx64abi7agjf"))
       (patches (search-patches "gajim-honour-GAJIM_PLUGIN_PATH.patch"))))
    (arguments
     `(#:imported-modules
       (,@%python-build-system-modules
        (guix build glib-or-gtk-build-system))
       #:modules
       ((guix build python-build-system)
        ((guix build glib-or-gtk-build-system)
         #:prefix glib-or-gtk:)
        (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             ;; ModuleNotFoundError: No module named 'gajim.gui.emoji_data'
             ;; https://dev.gajim.org/gajim/gajim/-/issues/10478
             (delete-file "test/lib/gajim_mocks.py")
             (delete-file "test/unit/test_gui_interface.py")
             #t))
         (replace 'check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1")
             ;; For missing '/etc/machine-id'.
             (setenv "DBUS_FATAL_WARNINGS" "0")
            ;(invoke "dbus-launch" "python" "./setup.py" "test")
             #t))
         (add-after 'wrap 'glib-or-gtk-compile-schemas
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
         (add-after 'wrap 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap))
         (add-after 'wrap 'wrap-env
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each
                (lambda (name)
                  (let ((file (string-append out "/bin/" name))
                        (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH"))
                        (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
                    (wrap-program file
                      `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))
                      `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))))
                '("gajim" "gajim-remote" "gajim-history-manager")))
             #t))))
     ;(substitute-keyword-arguments (package-arguments gajim)
     ;                              ((#:phases phases)
     ;                               `(modify-phases %standard-phases
     ;                                               (delete 'check))))
      )
    (propagated-inputs (cons*
                         `("gajim-omemo" ,gajim-omemo)
                         `("gajim-openpgp" ,gajim-openpgp)
                         `("gajim-pgp" ,gajim-pgp)
                         `("gajim-plugin-installer" ,gajim-plugin-installer)
                         `("libsoup" ,libsoup)
                         `("gtk+" ,gtk+)
                         `("gtksourceview" ,gtksourceview)
                         `("gst-plugins-base" ,gst-plugins-base)
                         (package-propagated-inputs gajim)))
    (inputs (cons*
              `("libsoup" ,libsoup)
              `("gtk+" ,gtk+)
              (assoc-set! (package-inputs gajim)
                          "python-nbxmpp"
                          (list
                            (package
                              (inherit python-nbxmpp)
                              (version "2.0.2-git")
                              (source
                                (origin
                                  (method git-fetch)
                                  (uri (git-reference
                                         (url "https://dev.gajim.org/gajim/python-nbxmpp.git")
                                         (commit "589fd69983bac85a6947004e010f3e22b5cb5053")))
                                  (sha256
                                    (base32 "08mdkabh16x76idhd51vs93r7c5h1ra8pj94iiirf6dphz44sri6")))))))))))

gajim-full
