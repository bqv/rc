(define-module (rc system delta)
               #:use-module (srfi srfi-1)
               #:use-module (gcrypt pk-crypto)
               #:use-module (guix packages)
               #:use-module (gnu)
               #:use-module (gnu system nss)
               #:use-module (nongnu system linux-initrd)
               #:use-module (gnu services desktop)
               #:use-module (gnu services sddm)
               #:use-module (gnu services shepherd)
               #:use-module (gnu services sound)
               #:use-module (gnu services ssh)
               #:use-module (gnu services networking)
               #:use-module (gnu services nix)
               #:use-module (gnu services vpn)
               #:use-module (gnu services xorg)
               #:use-module (rc services biboumi)
               #:use-module (rc services home)
               #:use-module (rc services ipfs)
               #:use-module ((rc keys biboumi) #:prefix keys:biboumi/)
               #:use-module (gnu packages android)
               #:use-module (gnu packages linux)
               #:use-module (gnu packages certs)
               #:use-module (gnu packages gnome)
               #:use-module (gnu packages vim)
               #:use-module (gnu packages admin)
               #:use-module (gnu packages file)
               #:use-module (gnu packages ssh)
               #:use-module (gnu packages rust-apps)
               #:use-module (gnu packages tmux)
               #:use-module (gnu packages version-control)
               #:use-module (gnu packages shells)
               #:use-module (gnu packages networking)
               #:use-module (gnu packages compression)
               #:use-module (gnu packages cpio)
               #:use-module (gnu packages vpn)
               #:use-module (gnu packages python)
               #:use-module (gnu packages curl)
               #:use-module (gnu packages screen)
               #:use-module (gnu packages xorg)
               #:use-module (gnu packages wm)
               #:use-module (gnu packages ipfs)
               #:use-module (gnu packages web)
               #:use-module (gnu packages rsync)
               #:use-module (gnu packages gnupg)
               #:use-module (gnu packages xdisorg)
               #:use-module (gnu packages irc)
               #:use-module (nongnu packages linux)
               #:use-module (rc packages biboumi)
               #:use-module (rc packages minecraft)
               #:use-module (rc packages nix)
               #:use-module (rc packages pipewire)
               #:use-module (rc packages xmpppy)
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
  
   ;(mapped-devices
   ; (list (mapped-device
   ;        (source (uuid "12345678-1234-1234-1234-123456789abc"))
   ;        ;; The UUID is that returned by 'cryptsetup luksUUID'.
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
                    (comment "Data User")
                    (shell (file-append fish "/bin/fish"))
                    (supplementary-groups '("wheel" "netdev"
                                            "audio" "video"
                                            "adbusers")))
                  (user-account
                    (name "python")
                    (group "python")
                    (comment "Python Env")
                    (home-directory "/home/python")
                    (shell "/home/python/.guix-profile/bin/python")
                    (supplementary-groups '()))
                  (user-account
                    (name "minecraft")
                    (group "games")
                    (home-directory "/var/lib/minecraft"))
                  %base-user-accounts))
  
    (groups (cons* (user-group
                     (name "games")
                     (system? #t))
                   (user-group
                     (name "adbusers")
                     (system? #f))
                   (user-group
                     (name "python"))
                   %base-groups))
  
    (packages (cons*
                nss-certs vim htop mosh ripgrep tmux go-ipfs file iwd
                git git-crypt git-remote-gcrypt (list git "send-email")
                neovim sshfs tree curl screen jq gvfs wireguard efibootmgr
                sway stumpwm awesome xinit xterm setxkbmap rsync gnupg python
                fish fish-foreign-env netcat rofi python-wrapper
                %base-packages))
  
    (setuid-programs (cons*
                       #~(string-append #$opendoas "/bin/doas")
                       %setuid-programs))
 
    (sudoers-file (plain-file "sudoers" "\
                              root ALL=(ALL) ALL
                              %wheel ALL=(ALL) NOPASSWD:ALL\n"))
  
    (name-service-switch %mdns-host-lookup-nss)
  
    (services (cons* (service gnome-desktop-service-type)
                     (service xfce-desktop-service-type)
                     (service openssh-service-type
                              (openssh-configuration
                                (permit-root-login #t)
                                (openssh openssh-sans-x)))
                     (service nix-service-type
                              (nix-configuration
                                (package nixUnstable)
                                (extra-config
                                  (list
                                    "experimental-features = nix-command flakes ca-references recursive-nix"
                                    "show-trace = true"))))
                     (service ipfs-service-type
                              (ipfs-configuration
                                (migrate #t)
                                (mount #f)
                                (args '("--enable-pubsub-experiment"
                                        "--enable-namesys-pubsub"))))
                     (service nftables-service-type
                              (nftables-configuration
                                (ruleset
                                  (plain-file "ruleset" 
                                              (let ((unlines
                                                      (lambda* (#:rest l)
                                                               (string-join l "\n"))))
                                              (unlines
                                                "table inet filter {"
                                                (unlines
                                                  "chain input {"
                                                  (unlines
                                                    "type filter hook input priority filter; policy drop;"
                                                    "ct state invalid drop"
                                                    "ct state { established, related } accept"
                                                    "iifname \"lo\" accept"
                                                    "ip protocol icmp accept"
                                                    "ip6 nexthdr ipv6-icmp accept"
                                                    "tcp dport 22 accept"
                                                    "udp dport 60000-65535 accept"
                                                    "accept");"reject")
                                                  "}")
                                                (unlines
                                                  "chain forward {"
                                                  (unlines
                                                    "type filter hook forward priority filter; policy drop;")
                                                  "}")
                                                (unlines
                                                  "chain output {"
                                                  (unlines
                                                    "type filter hook output priority filter; policy accept;")
                                                  "}")
                                                "}"))))))
                     (simple-service 'weechat shepherd-root-service-type
                                     (list (shepherd-service
                                             (documentation "Run the weechat daemon.")
                                             (provision '(weechat))
                                             (requirement '(networking))
                                             (start #~(make-forkexec-constructor
                                                        (list #$(file-append weechat "/bin/weechat-headless")
                                                              "-d" "/var/lib/weechat")
                                                        #:environment-variables
                                                        (append
                                                          (list
                                                            (string-append
                                                              "PYTHONPATH="
                                                              (string-join
                                                                (list
                                                                  #$(file-append python-xmpppy
                                                                                 "/lib/python3.8/site-packages"))
                                                                ":"))
                                                            "SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt"
                                                            "SSL_CERT_DIR=/etc/ssl/certs")
                                                          (environ))))
                                             (stop #~(make-kill-destructor))
                                             (respawn? #t))))
                    ;(service wpa-supplicant-service-type
                    ;         (wpa-supplicant-configuration
                    ;           (interface "wlo1")
                    ;           (config-file "/etc/wpa_supplicant/wpa_supplicant.conf")))
                     (service sddm-service-type
                              (sddm-configuration
                                (auto-login-user "leaf")
                                (auto-login-session "sway.desktop")
                                (xorg-configuration
                                  (xorg-configuration
                                    (keyboard-layout keyboard-layout)))))
                    ;(static-networking-service "enp4s0u1" "192.168.178.252"
                    ;                           #:gateway "192.168.178.1"
                    ;                           #:name-servers '("9.9.9.9"))
                     (service dhcp-client-service-type)
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
                     (service bluetooth-service-type)
                     (service biboumi-service-type
                              (biboumi-configuration
                                (user "biboumi")
                                (home "/tmp")
                                (config
                                  (mixed-text-file "biboumi.cfg"
                                                   (let ((biboumi-password keys:biboumi/password))
                                                     #~(string-join
                                                         (map (lambda (p) (string-append
                                                                            (symbol->string (car p))
                                                                            "="
                                                                            (if (number? (cdr p))
                                                                                (number->string (cdr p))
                                                                                (cdr p))))
                                                              `((admin . "qy@xa0.uk")
                                                                (ca_file . "/etc/ssl/certs/ca-certificates.crt")
                                                                (db_name . "/var/lib/biboumi/biboumi.sqlite")
                                                                (hostname . "irc.xa0.uk")
                                                                (identd_port . 113)
                                                                (log_level . 1)
                                                                (password . ,#$biboumi-password)
                                                                (persistent_by_default . "false")
                                                                (policy_directory . ,(string-append #$biboumi "/etc/biboumi"))
                                                                (port . 5347)
                                                                (realname_customization . "true")
                                                                (realname_from_jid . "false")
                                                                (xmpp_server_ip . "10.0.0.1")))
                                                         "\n"))))))
                     (udev-rules-service 'pipewire-add-udev-rules
                                         pipewire-next)
                     (udev-rules-service 'android-add-udev-rules
                                         android-udev-rules)
                     (simple-service 'minecraft-server shepherd-root-service-type
                                     (list (shepherd-service
                                             (documentation "Minecraft Server.")
                                             (provision '(minecraft))
                                             (requirement '(networking))
                                             (start #~(lambda _
                                                        (let ((mc (string-append #$minecraft-server
                                                                                 "/bin/minecraft-server"))
                                                              (user (getpwnam "minecraft")))
                                                          (mkdir-p "/var/lib/minecraft")
                                                          (chmod "/var/lib/minecraft" #o755)
                                                          (chown "/var/lib/minecraft"
                                                                 (passwd:uid user) (passwd:gid user))
                                                          (chdir "/var/lib/minecraft")
                                                          ;; /nix/store/41zy3hnpbd1rnfxc72h7mb1xjj78rh3i-unit-script-minecraft-server-pre-start/bin/minecraft-server-pre-start
                                                          ;;  -> https://gateway.ipfs.io/ipfs/QmbzcDZzxFDggQcKNGwRCDQvVqZovUvVDMq6nnMbYjboZs
                                                          (fork+exec-command
                                                            (list mc "-Xmx2048M" "-Xms2048M")
                                                            #:user (passwd:uid user)
                                                            #:group (passwd:gid user))))))))
                     (simple-service 'no-eth shepherd-root-service-type
                                     (list (shepherd-service
                                             (documentation "Set enp4s0u1 link down.")
                                             (provision '(no-eth))
                                             (requirement '(networking))
                                             (start #~(lambda _
                                                        (let ((ip (string-append #$iproute "/sbin/ip")))
                                                          (system* ip "link" "set" "enp4s0u1" "down")
                                                          #t)))
                                             (one-shot? #t))))
                     (simple-service 'use-gnu-var session-environment-service-type
                                     `(("GUIX_STATE_DIRECTORY" . "/gnu/var")))
                     (extra-special-file
                       "/etc/doas.conf"
                       (plain-file "doas.conf"
                                   (string-join (list
                                                  "permit nopass keepenv root" ; allowed to do anything
                                                  "permit nopass setenv { SSH_AUTH_SOCK } :wheel"
                                                  "")
                                                "\n")))
                     (service home-service-type
                              (home-configuration
                                (user "leaf")))
                     (modify-services
                       %desktop-services
                       (delete gdm-service-type)
                       (delete network-manager-service-type)
                       (delete pulseaudio-service-type)
                       (delete alsa-service-type)
                       (guix-service-type config =>
                                          (guix-configuration
                                            (inherit config)
                                            (substitute-urls
                                              (append
                                                (list "https://bordeaux.guix.gnu.org"
                                                      "https://mirror.brielmaier.net")
                                                %default-substitute-urls))
                                            (authorized-keys
                                              (append
                                                (list (plain-file
                                                        "bordeaux.guix.gnu.org.pub"
                                                        (canonical-sexp->string
                                                          (sexp->canonical-sexp
                                                            '(public-key
                                                               (ecc
                                                                 (curve Ed25519)
                                                                 (q #vu8(125 96 41 2 211 162 219 184
                                                                         63 138 15 185 134 2 167 84
                                                                         197 73 59 11 119 140 141 29
                                                                         212 224 244 29 225 77 227 79))))
                                                            )))
                                                      (plain-file
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
