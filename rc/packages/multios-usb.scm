(define-module (rc packages multios-usb)
               #:use-module (guix git-download)
               #:use-module (guix utils)
               #:use-module (guix packages)
               #:use-module (guix build-system copy)
               #:use-module (gnu packages bash)
               #:use-module (gnu packages bootloaders)
               #:use-module (gnu packages disk)
               #:use-module (gnu packages linux)
               #:export (multios-usb))

(define multios-usb
  (package
    (name "multios-usb")
    (version "0.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/Mexit/MultiOS-USB")
                     (commit (string-append "v" version))))
              (sha256
                (base32
                  "15lm5r4mq7165q6q169lgxhhh3y3xf0afdmr75xqb35bs2wv5q1f"))))
    (inputs `(("bash" ,bash)
              ("e2fsprogs" ,e2fsprogs)
              ("dosfstools" ,dosfstools)
              ("grub" ,grub)))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("." "libexec"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'clean
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bash (assoc-ref inputs "bash"))
                    (binpath
                      (append
                        (map (lambda (i) (string-append (cdr i) "/sbin")) inputs)
                        (map (lambda (i) (string-append (cdr i) "/bin")) inputs))))
               (mkdir-p (string-append out "/bin"))
               (with-output-to-file
                 (string-append out "/bin/multios-usb-install")
                 (lambda _
                   (display (string-append "#!" (string-append bash "/bin/sh") "\n"))
                   (display (string-append "export PATH=" (string-join binpath ":") ":$PATH\n"))
                   (display (string-append "RUNDIR=$(mktemp -d /tmp/.multios-usb.XXX)\n"))
                   (display (string-append "cp -r " (string-append out "/libexec/* ") "$RUNDIR\n"))
                   (display (string-append "cd $RUNDIR\n"))
                   (display (string-append "source " (string-append out "/libexec/installer.sh ") "$@\n"))
                   (display (string-append "rm -rf $RUNDIR\n"))
                   (newline)))
               (chmod (string-append out "/bin/multios-usb-install") #o555)
               (with-output-to-file
                 (string-append out "/bin/multios-usb-update")
                 (lambda _
                   (display (string-append "#!" (string-append bash "/bin/sh") "\n"))
                   (display (string-append "export PATH=" (string-join binpath ":") ":$PATH\n"))
                   (display (string-append "RUNDIR=$(mktemp -d /tmp/.multios-usb.XXX)\n"))
                   (display (string-append "cp -r " (string-append out "/libexec/* ") "$RUNDIR\n"))
                   (display (string-append "cd $RUNDIR\n"))
                   (display (string-append "source " (string-append out "/libexec/updater.sh ") "$@\n"))
                   (display (string-append "rm -rf $RUNDIR\n"))
                   (newline)))
               (chmod (string-append out "/bin/multios-usb-update") #o555)
               #t))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

multios-usb
