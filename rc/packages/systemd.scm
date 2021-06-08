;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyleft @ 2021 bqv <bqv@fron.io>

(define-module (rc packages systemd)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system meson)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml))

(define-public systemd
  (package
    (name "systemd")
    (version "247")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/systemd/systemd/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                 "0h66cq7ymm419cjyg0gla18ni4y8qf9fsav9ysgacjik4xxny53p"))))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f             ;FIXME: The disable-broken-tests phase is ineffective.
       #:configure-flags
       (let ((bash            (assoc-ref %build-inputs "bash"))
             (coreutils       (assoc-ref %build-inputs "coreutils"))
             (kbd             (assoc-ref %build-inputs "kbd"))
             (kmod            (assoc-ref %build-inputs "kmod"))
             (util-linux      (assoc-ref %build-inputs "util-linux"))

             (out (assoc-ref %outputs "out")))
         (list (string-append "-Dkill-path=" coreutils "/bin/kill")
               (string-append "-Dkmod-path=" kmod "/bin/kmod")
               (string-append "-Dsulogin-path=" util-linux "/bin/sulogin")
               (string-append "-Dmount-path=" util-linux "/bin/mount")
               (string-append "-Dumount-path=" util-linux "/bin/umount")
               (string-append "-Dloadkeys-path=" kbd "/bin/loadkeys")
               (string-append "-Dsetfont-path=" kbd "/bin/setfont")
               (string-append "-Ddebug-shell=" bash "/bin/sh")

               ;; XXX: Can we reuse %ntp-servers here?
               (string-append "-Dntp-servers="
                              (string-join (map (lambda (n)
                                                  (string-append (number->string n)
                                                                 ".guix.pool.ntp.org"))
                                                '(0 1 2 3))
                                           ","))

               ;; Use localhost for DNS with fallback to Quad9 (instead of Google).
               "-Ddns-servers=127.0.0.1,::1,9.9.9.10,2620:fe::10"

               ;; FIXME: "Attempt to load external entity http://docbook.sf.net".
               "-Dman=false"

               ;; Don't install SysV compatibility scripts.
               "-Dsysvinit-path="
               "-Dsysvrcnd-path="

               (string-append "-Dbashcompletiondir=" out "/etc/bash_completion.d")
               (string-append "-Dsysconfdir=" out "/etc")
               (string-append "-Drootprefix=" out)
               (string-append "-Drootlibdir=" out "/lib")
               (string-append "-Ddbuspolicydir=" out "/etc/dbus-1/system.d")
               (string-append "-Dpamconfdir=" out "/etc/pam.d")))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-paths
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((bash            (assoc-ref inputs "bash"))
                            (coreutils       (assoc-ref inputs "coreutils"))
                            (glibc           (assoc-ref inputs "glibc"))
                            (util-linux      (assoc-ref inputs "util-linux"))

                            (out (assoc-ref outputs "out")))

                        (substitute* '("src/core/swap.c"
                                       "src/fsck/fsck.c"
                                       "src/journal/cat.c"
                                       "src/nspawn/nspawn.c"
                                       "src/nspawn/nspawn-setuid.c")
                          (("/bin/sh") (string-append bash "/bin/sh"))
                          (("/bin/bash") (string-append bash "/bin/bash"))
                          (("/bin/cat") (string-append coreutils "/bin/cat"))
                          (("/bin/echo") (string-append coreutils "/bin/echo"))
                          (("/bin/getent") (string-append glibc "/bin/getent"))
                          (("/sbin/fsck") (string-append util-linux "/sbin/fsck"))
                          (("/sbin/swapon") (string-append util-linux "/sbin/swapon"))
                          (("/sbin/swapoff") (string-append util-linux "/sbin/swapoff")))
                        (substitute* "src/journal/catalog.c"
                          (("/usr/lib/systemd/catalog")
                           (string-append out "/lib/systemd/catalog")))
                        #t)))
                  (add-after 'patch-paths 'fix-install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        ;; Don't create /var/lib/systemd during install.
                        (substitute* "meson.build"
                          ((".*mkdir_p\\.format\\(systemdstatedir\\)\\)") ""))
                        ;; Nor /var/lib/systemd/catalog.
                        (substitute* "catalog/meson.build"
                          (("journalctl --update-catalog") "journalctl --version"))
                        ;; Likewise for /var/log/journal.
                        (substitute* '("src/journal/meson.build"
                                       "src/journal-remote/meson.build")
                          (("/var/log/journal") "/tmp/journal"))
                        ;; Create the hwdb in out/etc/udev/hwdb.d, not /etc/udev.
                        (substitute* "hwdb.d/meson.build"
                          (("systemd-hwdb update")
                           (string-append "systemd-hwdb -r " out
                                          "/etc/udev/hwdb.d update")))
                        #t)))
                  (add-before 'configure 'set-runpath
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        ;; We need out/lib and out/lib/systemd in RUNPATH.
                        (setenv "LDFLAGS" (string-append "-Wl,-rpath=" out "/lib,"
                                                         "-rpath=" out "/lib/systemd"))
                        #t)))
                  (add-before 'check 'disable-broken-tests
                    (lambda _
                      (delete-file "test-network")           ;requires loopback
                      (delete-file "test-engine")            ;requires cgroups
                      (delete-file "test-unit-name")         ;likewise
                      (delete-file "test-unit-file")         ;likewise
                      (delete-file "test-copy")              ;FIXME
                      (delete-file "test-condition")         ;requires containers
                      (delete-file "test-mount-util")        ;requires /sys
                      (delete-file "test-exec-util")         ;FIXME
                      (delete-file "test-xattr-util")        ;FIXME
                      (delete-file "test-fs-util")           ;requires /var/tmp
                      (delete-file "test-stat-util")         ;FIXME
                      (delete-file "test-user-util")         ;needs "root" user
                      (delete-file "test-path-lookup")       ;expects systemd paths
                      (delete-file "test-namespace")         ;requires containers
                     ;(delete-file "test-bpf")               ;requires cgroups
                      (delete-file "test-fileio")            ;FIXME
                      (delete-file "test-time-util")         ;FIXME tzdata
                      (delete-file "test-date")              ;likewise
                      (delete-file "test-calendarspec")      ;likewise
                      (delete-file "test-cgroup-util")       ;requires cgroup (duh)
                      (delete-file "test-strv")              ;FIXME
                      (delete-file "test-path-util")         ;FIXME /bin/sh
                      (delete-file "test-path")              ;requires cgroup
                      (delete-file "test-sched-prio")        ;requires cgroup
                      (delete-file "test-id128")             ;FIXME
                      (delete-file "test-journal-flush")     ;FIXME
                      (delete-file "test-bus-creds")         ;requires cgroup
                      (delete-file "test-login")             ;FIXME
                      (delete-file "test-dhcp-client")       ;requires network
                      (delete-file "test-dhcp6-client")      ;likewise
                      #t))
                  (add-before 'install 'fix-install
                    (lambda _
                      (setenv "DESTDIR" "/")
                      #t))
                  (add-after 'install 'fix-environment-symlink
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; The install phase creates this dangling symlink:
                      ;; lib/environment.d/00-environment.conf -> ../../etc/environment
                      ;; ...which causes the 'fix-runpath' phase to error out.
                      ;; XXX: This should probably use /etc/environment instead.
                      (mkdir-p (string-append (assoc-ref outputs "out")
                                              "/etc/environment"))
                      #t))
                  (add-before 'strip 'add-shared-lib
                    (lambda* (#:key outputs #:allow-other-keys)
                      (link (string-append (assoc-ref outputs "out")
                                           "/lib/systemd/libsystemd-shared-247.so")
                            (string-append (assoc-ref outputs "out")
                                           "/lib/libsystemd-shared-247.so"))
                      #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml)
       ("docbook-xsl" ,docbook-xsl)
       ("gettext" ,gettext-minimal)
       ("gperf" ,gperf)
       ("lxml" ,python-lxml)
       ("m4" ,m4)
       ("pkg-config" ,pkg-config)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("acl" ,acl)
       ("audit" ,audit)
       ("bash" ,bash)
       ("bzip2" ,bzip2)
       ("coreutils" ,coreutils)
       ("cryptsetup" ,cryptsetup)
       ("curl" ,curl)
       ("dbus" ,dbus)
       ;; TODO: Add gnu-efi for bootloader functionality.
       ("elfutils" ,elfutils)
       ("glib" ,glib)
       ("glibc" ,glibc)
       ("gnutls" ,gnutls)
       ("kbd" ,kbd)
       ("kmod" ,kmod)
       ("libcap" ,libcap)
       ("libgcrypt" ,libgcrypt)
       ("libidn2" ,libidn2)
       ("libmicrohttpd" ,libmicrohttpd)
       ("libseccomp" ,libseccomp)
       ("libxkbcommon" ,libxkbcommon)
       ("linux-pam" ,linux-pam)
       ("lz4" ,lz4)
       ("pcre2" ,pcre2)
       ("python" ,python)
       ("qrencode" ,qrencode)
       ("util-linux" ,util-linux)
       ("xz" ,xz)
       ("zlib" ,zlib)))
    (home-page "https://www.freedesktop.org/wiki/Software/systemd/")
    (synopsis "System and service manager")
    (description
     "@code{systemd} is a suite of basic building blocks for a Linux system.
It provides a system and service manager that runs as PID 1 and starts the
rest of the system.  systemd provides aggressive parallelization capabilities,
uses socket and D-Bus activation for starting services, offers on-demand
starting of daemons, keeps track of processes using Linux control groups,
maintains mount and automount points, and implements an elaborate transactional
dependency-based service control logic.")
    (license license:lgpl2.1+)))
