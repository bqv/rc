(use-modules (gnu))
(use-modules (gnu packages linux))
(use-modules (gnu system locale))
(use-modules (nongnu packages linux))
(use-modules (nongnu system linux-initrd))
(use-modules (guix packages))
(use-modules (guix channels))
(use-modules (guix inferior))
(use-modules (srfi srfi-1))
(use-service-modules networking ssh xorg sddm desktop)
(use-package-modules
 admin networking certs compression python cpio
 web-browsers xorg curl version-control emacs-xyz
 shells wm guile-wm vpn emacs vim tmux screen)

(load "emacs-webkit.scm")

(operating-system
  (host-name "epsilon")
  (timezone "Europe/London")
  (locale "en_GB.UTF-8")
 ;(locale-definitions (list (locale-definition
 ;                            (name "en_GB.UTF-8") (source "en_GB"))
 ;                          (locale-definition
 ;                            (name "en_US.UTF-8") (source "en_US"))))
  (keyboard-layout (keyboard-layout "gb" #:options '("ctrl:nocaps")))
  (kernel
    (let* ()
      (package
            (inherit linux)
            (native-inputs
              `(("kconfig" ,(local-file "kconfig"))
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

  ;; The "root" account is implicit, initially created with empty password.
  (users (cons* (user-account
                 (name "aion")
                 (group "users")
                 ;; Adding the account to the "wheel" group
                 ;; makes it a sudoer.
                 (supplementary-groups '("wheel"))
                 (shell (file-append fish "/bin/fish"))
                 (home-directory "/home/aion"))
               %base-user-accounts))
  (packages (cons*
              emacs-next neovim vim
	      nyxt xterm sshfs tree curl git
              tmux screen htop tcpdump
              guile-wm stumpwm wireguard nss-certs
              emacs-evil emacs-ivy emacs-vterm ;emacs-webkit
              %base-packages))
  (sudoers-file (plain-file "sudoers" "\
                            root ALL=(ALL) ALL
                            %wheel ALL=NOPASSWD: ALL\n"))
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
                                `(("root" ,(local-file "aion.pub"))
                                  ("aion" ,(local-file "aion.pub"))))))
              %base-services)))
