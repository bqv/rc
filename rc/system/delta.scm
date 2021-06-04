(define-module (rc system delta)
	       #:use-module (gnu)
	       #:use-module (gnu packages linux)
	       #:use-module (gnu system nss)
	       #:use-module (nongnu packages linux)
	       #:use-module (nongnu packages mozilla)
	       #:use-module (nongnu system linux-initrd)
	       #:use-module (srfi srfi-1)
	       #:use-module (gnu services desktop)
	       #:use-module (gnu services xorg)
	       #:use-module (gnu services sddm)
	       #:use-module (gnu services shepherd)
	       #:use-module (gnu services ssh)
	       #:use-module (gnu services networking)
	       #:use-module (gnu services nix)
	       #:use-module (gnu services vpn)
	       #:use-module (gnu packages certs)
	       #:use-module (gnu packages gnome)
	       #:use-module (gnu packages vim)
	       #:use-module (gnu packages admin)
	       #:use-module (gnu packages chromium)
	       #:use-module (gnu packages file)
	       #:use-module (gnu packages ssh)
	       #:use-module (gnu packages rust-apps)
	       #:use-module (gnu packages tmux)
	       #:use-module (gnu packages dvtm)
	       #:use-module (gnu packages version-control)
	       #:use-module (gnu packages shells)
	       #:use-module (gnu packages networking)
	       #:use-module (gnu packages compression)
	       #:use-module (gnu packages cpio)
	       #:use-module (gnu packages vpn)
	       #:use-module (gnu packages python)
	       #:use-module (gnu packages web-browsers)
	       #:use-module (gnu packages curl)
	       #:use-module (gnu packages screen)
	       #:use-module (gnu packages emacs)
	       #:use-module (gnu packages emacs-xyz)
	       #:use-module (gnu packages xorg)
	       #:use-module (gnu packages wm)
	       #:use-module (gnu packages ipfs)
	       #:use-module (gnu packages messaging)
	       #:use-module (gnu packages irc)
	       #:use-module (gnu packages web)
	       #:use-module (gnu packages rsync)
	       #:use-module (gnu packages gnupg)
	       #:use-module (gnu packages terminals)
	       #:export (os))

(define (os)
  (operating-system
    (host-name "delta")
    (timezone "Europe/London")
    (locale "en_GB.utf8")
  
    (keyboard-layout (keyboard-layout "gb" #:options '("ctrl:nocaps")))
  
    (bootloader (bootloader-configuration
                  (bootloader grub-efi-bootloader)
                  (target "/boot/EFI")
                  (terminal-outputs '(gfxterm vga_text console))
                  (keyboard-layout keyboard-layout)))
  
    (kernel linux)
    (kernel-arguments (cons*;"nomodeset"
                             "i915.modeset=0"
                             "modprobe.blacklist=pcspkr"
                             (delete "quiet" %default-kernel-arguments)))
    (initrd microcode-initrd)
    (initrd-modules (cons*;"amdgpu"
                          ;"i915"
                           %base-initrd-modules))
    (firmware (cons* amdgpu-firmware linux-firmware
                     %base-firmware))
  
    ;; Specify a mapped device for the encrypted root partition.
    ;; The UUID is that returned by 'cryptsetup luksUUID'.
   ;(mapped-devices
   ; (list (mapped-device
   ;        (source (uuid "12345678-1234-1234-1234-123456789abc"))
   ;        (target "my-root")
   ;        (type luks-device-mapping))))
  
    (file-systems (let ((hdd (uuid "7aebd443-ae06-4ef4-927b-fb6816ef175b"))
                        (ssd (uuid "3bfa9fa3-46f5-47fa-b9bb-d2ba05801c09"))
                        (boot (uuid "4305-4121" 'fat)))
                    (cons* (file-system (device hdd)
                                        (mount-point "/")
                                       ;(dependencies mapped-devices)
                                        (needed-for-boot? #t)
                                        (type "btrfs")
                                        (options "subvol=guixsd"))
                           (file-system (device hdd)
                                        (mount-point "/home")
                                        (needed-for-boot? #t)
                                        (type "btrfs")
                                        (options "subvol=home"))
                           (file-system (device hdd)
                                        (mount-point "/var")
                                        (needed-for-boot? #t)
                                        (type "btrfs")
                                        (options "subvol=var"))
                           (file-system (device hdd)
                                        (mount-point "/srv")
                                        (type "btrfs")
                                        (options "subvol=srv"))
                           (file-system (device hdd)
                                        (mount-point "/games")
                                        (mount-may-fail? #t)
                                        (type "btrfs")
                                        (options "subvol=games"))
                           (file-system (device ssd)
                                        (mount-point "/gnu")
                                        (needed-for-boot? #t)
                                        (flags '(no-atime))
                                        (options "subvol=gnu")
                                        (type "btrfs"))
                           (file-system (device ssd)
                                        (mount-point "/nix")
                                        (flags '(no-atime))
                                        (options "subvol=nix")
                                        (mount-may-fail? #t)
                                        (type "btrfs"))
                           (file-system (device boot)
                                        (mount-point "/boot")
                                        (needed-for-boot? #t)
                                        (type "vfat"))
                           %base-file-systems)))
  
    (users (cons* (user-account
                    (name "leaf")
                    (password (crypt "alice" "$6$abc"))
                    (group "users")
                    (shell (file-append fish "/bin/fish"))
                    (supplementary-groups '("wheel" "netdev"
                                            "audio" "video")))
                  %base-user-accounts))
  
    (packages (cons*
                nss-certs ;; for HTTPS access
                gvfs ;; for user mounts
                vim htop firefox mosh ripgrep tmux dvtm git go-ipfs file
                emacs-next neovim nyxt xterm sshfs tree curl screen jq
                stumpwm wireguard emacs-evil emacs-ivy emacs-vterm emacs-geiser
                efibootmgr dino weechat xinit irssi profanity poezio
		ungoogled-chromium fish fish-foreign-env netcat
		xinit setxkbmap rsync gnupg sway awesome termite
                %base-packages))
  
    (setuid-programs (cons*
                       #~(string-append #$opendoas "/bin/doas")
                       %setuid-programs))
    (sudoers-file (plain-file "sudoers" "\
                              root ALL=(ALL) ALL
                              %wheel ALL=(ALL) NOPASSWD:ALL\n"))
  
    ;; Add GNOME and Xfce---we can choose at the log-in screen
    ;; by clicking the gear.  Use the "desktop" services, which
    ;; include the X11 log-in service, networking with
    ;; NetworkManager, and more.
    (services (cons* (service gnome-desktop-service-type)
                     (service xfce-desktop-service-type)
                     (service openssh-service-type
                              (openssh-configuration
                                (permit-root-login #t)
                                (openssh openssh-sans-x)))
                     (service nix-service-type
                              (nix-configuration
                                (extra-config
                                  (list
                                    "experimental-features = nix-command flakes ca-references recursive-nix"
                                    "show-trace = true"))))
                     (service ipfs-service-type
                              (ipfs-configuration))
                     (service nftables-service-type
                              (nftables-configuration
                                (ruleset (plain-file "ruleset" "\
  table inet filter {
          chain input {
                  type filter hook input priority filter; policy drop;
                  ct state invalid drop
                  ct state { established, related } accept
                  iifname \"lo\" accept
                  ip protocol icmp accept
                  ip6 nexthdr ipv6-icmp accept
                  tcp dport 22 accept
                  udp dport 60000-65535 accept
  		accept
                  reject
          }
  
          chain forward {
                  type filter hook forward priority filter; policy drop;
          }
  
          chain output {
                  type filter hook output priority filter; policy accept;
          }
  }
                                                               "))))
                     (simple-service 'weechat shepherd-root-service-type
                                     (list (shepherd-service
                                             (documentation "Run the weechat daemon.")
                                             (provision '(weechat))
                                             (requirement '(networking))
                                             (start #~(make-forkexec-constructor
                                                        (list #$(file-append weechat "/bin/weechat-headless")
                                                              "-d" "/var/lib/weechat")))
                                             (stop #~(make-kill-destructor)))))
                    ;(service wpa-supplicant-service-type
                    ;         (wpa-supplicant-configuration
                    ;           (interface "wlo1")
                    ;           (config-file "/etc/wpa_supplicant/wpa_supplicant.conf")))
                     (service sddm-service-type
                              (sddm-configuration
                               ;(auto-login-user "aion")
                               ;(auto-login-session "stumpwm.desktop")
                                (xorg-configuration
                                  (xorg-configuration
                                    (keyboard-layout keyboard-layout)))))
                    ;(service elogind-service-type
                    ;         (elogind-configuration))
  		   (service wireguard-service-type
  			    (wireguard-configuration
  			      (addresses '("10.0.0.3/24"))
  			      (peers
  				(list
  				  (wireguard-peer
  				    (name "zeta")
  				    (endpoint "163.172.7.233:51820")
  				    (public-key "WbZqPcgSxWf+mNsWVbS+0JylysN9FKrRG9783wn1JAg=")
  				    (allowed-ips '("10.0.0.1/32"))
  				    (keep-alive 10))
  				  (wireguard-peer
  				    (name "theta")
  				    (endpoint "192.168.178.101:51820")
  				    (public-key "Itld9S83/URY8CR1ZsIfYRGK74/T0O5YbsHWcNpn2gE=")
  				    (allowed-ips '("10.0.0.2/32"))
  				    (keep-alive 10))
  				  (wireguard-peer
  				    (name "phi")
  				    (endpoint "192.168.178.135:51820")
  				    (public-key "kccZA+GAc0VStb28A+Kr0z8iPCWsiuRMfwHW391Qrko=")
  				    (allowed-ips '("10.0.0.4/32"))
  				    (keep-alive 10))))))
  		   (simple-service 'no-eth shepherd-root-service-type
                                     (list (shepherd-service
                                             (documentation "Set enp4s0u1 link down.")
                                             (provision '(no-eth))
                                             (requirement '(networking))
                                             (start #~(lambda _
                                                        (let ((ip (string-append #$iproute "/sbin/ip")))
                                                          (invoke ip "link" "set" "enp4s0u1" "down"))))
                                             (one-shot? #t))))
                     (extra-special-file "/etc/doas.conf"
                                         (plain-file "doas.conf" "\
  # 'root' is allowed to do anything.
  permit nopass keepenv root
  
  permit nopass    setenv { SSH_AUTH_SOCK  } :wheel   
                                                                 "))
                     (modify-services
                       %desktop-services
                       (delete gdm-service-type)
  		     (guix-service-type config =>
  					(guix-configuration
  					  (inherit config)
  					  (substitute-urls
  					    (append (list "https://mirror.brielmaier.net")
  						    %default-substitute-urls))
  					  (authorized-keys
  					    (append (list (plain-file "key.pub" "\
  (public-key 
   (ecc 
    (curve Ed25519)
    (q #7514F8D729DB1935470A581CE3851ED9FD6F1F9BAFE1D8BEC77A931ADB7A4337#)
    )
   )
  								                "))
  						    %default-authorized-guix-keys)))))))
  
    ;; Allow resolution of '.local' host names with mDNS.
    (name-service-switch %mdns-host-lookup-nss)))
