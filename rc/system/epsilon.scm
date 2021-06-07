(define-module (rc system epsilon)
	       #:use-module (gnu)
	       #:use-module (gnu packages linux)
	       #:use-module (gnu system locale)
	       #:use-module (nongnu packages linux)
	       #:use-module (nongnu system linux-initrd)
	       #:use-module (guix packages)
	       #:use-module (guix channels)
	       #:use-module (guix inferior)
	       #:use-module (srfi srfi-1)
	       #:use-module (gcrypt pk-crypto)
	       #:use-module (gnu services networking)
	       #:use-module (gnu services ssh)
	       #:use-module (gnu services xorg)
	       #:use-module (gnu services sddm)
	       #:use-module (gnu services desktop)
	       #:use-module (gnu packages admin)
	       #:use-module (gnu packages networking)
	       #:use-module (gnu packages certs)
	       #:use-module (gnu packages compression)
	       #:use-module (gnu packages python)
	       #:use-module (gnu packages cpio)
	       #:use-module (gnu packages web-browsers)
	       #:use-module (gnu packages xorg)
	       #:use-module (gnu packages curl)
	       #:use-module (gnu packages version-control)
	       #:use-module (gnu packages emacs-xyz)
	       #:use-module (gnu packages shells)
	       #:use-module (gnu packages wm)
	       #:use-module (gnu packages guile-wm)
	       #:use-module (gnu packages vpn)
	       #:use-module (gnu packages emacs)
	       #:use-module (gnu packages vim)
	       #:use-module (gnu packages tmux)
	       #:use-module (gnu packages screen)
	       #:use-module (gnu packages rsync)
	       #:use-module (gnu packages suckless)
	       #:use-module (gnu packages messaging)
	       #:export (os))

(define (os)
  (operating-system
    (host-name "epsilon")
    (timezone "Europe/London")
    (locale "en_GB.UTF-8")
    (locale-definitions (list (locale-definition
                                (name "en_GB.UTF-8") (source "en_GB"))
                              (locale-definition
                                (name "en_US.UTF-8") (source "en_US"))))
    (keyboard-layout (keyboard-layout "gb" #:options '("ctrl:nocaps")))
    (kernel
      (let* ()
        (package
          (inherit linux)
          (native-inputs
            `(("kconfig" ,(local-file "../../data/cyan-kconfig"))
              ("zlib" ,zlib)
              ("tar" ,tar)
              ("gzip" ,gzip)
              ("bzip2" ,bzip2)
              ("cpio" ,cpio)
              ("python" ,python)
              ("python-wrapper" ,python-wrapper)
              ,@(alist-delete "kconfig"
                              (package-native-inputs linux)))))))
    (kernel-arguments (list
                        "slub_debug=P"
                        "page_poison=1"
                        "tpm_tis.interrupts=0"
                        "apic_osi=Linux"))
    (initrd microcode-initrd)
    (initrd-modules (cons* "sdhci"
                           "sdhci_pci"
                           "mmc_core"
                           "mmc_block"
                           (delete "xts" %base-initrd-modules)))
    (firmware (list linux-firmware))
    (bootloader
      (bootloader-configuration
        (bootloader grub-efi-bootloader)
        (target "/boot")
        (keyboard-layout keyboard-layout)
        (menu-entries (list (menu-entry
                              (label "Void")
                              (device "/boot")
                              (linux "/vmlinuz-5.10.13_1")
                              (linux-arguments
                                (list "root=UUID=ee336d51-b46e-487c-b186-1f7a9cb6350f"
                                      "ro" "rootflags=subvol=@" "loglevel=4"
                                      "slub_debug=P" "page_poison=1"
                                      "tpm_tis.interrupts=0" "acpi_osi=Linux"))
                              (initrd "/initramfs-5.10.13_1.img"))
                            ))))
    (file-systems (cons* (file-system
                           (device (uuid "ee336d51-b46e-487c-b186-1f7a9cb6350f"))
                           (mount-point "/")
                           (options "subvol=@")
                           (needed-for-boot? #t)
                           (type "btrfs"))
                         (file-system
                           (device (uuid "ee336d51-b46e-487c-b186-1f7a9cb6350f"))
                           (mount-point "/home")
                           (options "subvol=@home")
                           (type "btrfs"))
                         (file-system
                           (device (uuid "FEEE-1F26" 'fat))
                           (mount-point "/boot")
                           (type "vfat"))
                         (file-system
                           (device (uuid "ee336d51-b46e-487c-b186-1f7a9cb6350f"))
                           (mount-point "/media")
                           (options "subvolid=0")
                           (type "btrfs"))
                         %base-file-systems))

    (users (cons* (user-account
                    (name "aion")
                    (group "users")
                    (supplementary-groups '("wheel"))
                    (shell (file-append fish "/bin/fish"))
                    (home-directory "/home/aion"))
                  %base-user-accounts))
    (packages (cons*
                emacs-next neovim vim neofetch
                nyxt xterm sshfs tree curl git netcat rsync
                tmux screen htop tcpdump st
                guile-wm stumpwm wireguard nss-certs
                emacs-evil emacs-ivy emacs-vterm ;emacs-webkit
                gajim gajim-omemo gajim-openpgp dino
                %base-packages))
    (sudoers-file (plain-file "sudoers" (string-join
                                          (list
                                            "root ALL=(ALL) ALL"
                                            "%wheel ALL=NOPASSWD: ALL"
                                            "")
                                          "\n")))
    (services (cons* (service dhcp-client-service-type)
                     (service wpa-supplicant-service-type
                              (wpa-supplicant-configuration
                                (interface "wlp2s0")
                                (config-file "/etc/wpa_supplicant/wpa_supplicant.conf")))
                     (service elogind-service-type
                              (elogind-configuration))
                     (service sddm-service-type
                              (sddm-configuration
                                (auto-login-user "aion")
                                (auto-login-session "stumpwm.desktop")
                                (xorg-configuration
                                  (xorg-configuration
                                    (keyboard-layout keyboard-layout)))))
                     (service openssh-service-type
                              (openssh-configuration
                                (permit-root-login #t)
                                (authorized-keys
                                  `(("root" ,(local-file "../../data/aion.pub"))
                                    ("aion" ,(local-file "../../data/aion.pub"))))))
                     (modify-services
                       %base-services
                       (guix-service-type config =>
                                          (guix-configuration
                                            (inherit config)
                                            (substitute-urls
                                              (append
                                                (list "https://mirror.brielmaier.net")
                                                %default-substitute-urls))
                                            (authorized-keys
                                              (append
                                                (list (plain-file
                                                        "mirror.brielmair.net.pub"
                                                        (canonical-sexp->string
                                                          (sexp->canonical-sexp
                                                            '(public-key
                                                               (ecc
                                                                 (curve Ed25519)
                                                                 (q #vu8(117 20 248 215 41 219 25 53
                                                                         71 10 88 28 227 133 30 217
                                                                         253 111 31 155 175 225 216 190
                                                                         199 122 147 26 219 122 67 55))))
                                                            ))))
                                                %default-authorized-guix-keys)))))))))
