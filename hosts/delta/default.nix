{ config, lib, pkgs, usr, flake, system, hosts, ... }:

{
  imports = [
    ../../profiles/meta/fatal-warnings.nix
    ../../profiles/misc/disable-mitigations.nix
    ../../profiles/misc/udev-nosettle.nix
    ../../profiles/misc/adblocking.nix
    ../../profiles/misc/odbc.nix
    ../../profiles/security/sudo.nix
   #../../profiles/security/apparmor
    ../../profiles/services/syncthing
    ../../profiles/services/aria2
    ../../profiles/services/guix
    ../../profiles/services/searx
    ../../profiles/services/hydra
    ../../profiles/networking/ipfs
    ../../profiles/networking/bluetooth
    ../../profiles/networking/wireguard
    ../../profiles/networking/mdns.nix
    ../../profiles/sound/pipewire.nix
    ../../profiles/virtualization/anbox
    ../../profiles/graphical
    ../../profiles/games
    ../../profiles/bcachefs.nix
    ../../profiles/wayland.nix
    ../../profiles/weechat.nix
    ../../users/root.nix
    ../../users/bao.nix
    ./xserver.nix
    ./network.nix
    ./remote.nix
  ];

  platform = "x86_64-linux";

  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    grub = {
      enable= true;
      device = "nodev";
     #efiInstallAsRemovable = true;
      efiSupport = true;
      memtest86.enable = true;
      useOSProber = true;
      configurationLimit = 64;
    };
    efi.canTouchEfiVariables = true;
    systemd-boot = {
      enable = false;
      configurationLimit = 64;
      memtest86.enable = true;
    };
  };

  boot.initrd.availableKernelModules = [
    "xhci_pci" "ehci_pci" "ahci" "usbcore"
    "sd_mod" "sr_mod" "nvme" "amdgpu"
  ];
  boot.initrd.kernelModules = [ "amdgpu" ];
  boot.initrd.secrets = {
    "/etc/nixos" = lib.cleanSource ./../..;
  };
  boot.kernelModules = [ "kvm-intel" "amdgpu" "fuse" ];
  boot.kernelParams = [ "mce=3" ];
  boot.extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback ];
  boot.binfmt.emulatedSystems = [ "armv7l-linux" "aarch64-linux" ];
  boot.postBootCommands = ''
   #echo 0000:04:00.0 > /sys/bus/pci/drivers/xhci_hcd/unbind
  ''; # usb4 is faulty
  boot.tmpOnTmpfs = false;

  fileSystems = let
    hdd = {
      device = "/dev/disk/by-uuid/f61d5c96-14db-4684-9bd6-218a468433b2";
      fsType = "btrfs";
    };
    ssd = {
      device = "/dev/sda2";
      fsType = "btrfs";
    };
  in {
    "/" = {
      device = "none";
      fsType = "tmpfs";
      options = [ "defaults" "size=2G" "mode=755" "nr_inodes=8M" ];
    };

    "/var" = hdd // { options = [ "subvol=var" ]; };
    "/home" = hdd // { options = [ "subvol=home" ]; };
    "/srv" = hdd // { options = [ "subvol=srv" ]; };
    "/nix" = ssd // { options = [ "noatime" "nodiratime" "discard=async" ]; };
    "/games" = hdd // { options = [ "subvol=games" ]; };
    "/run/hdd" = hdd // { options = [ "subvolid=0" ]; };
    "/run/ssd" = ssd // { options = [ "subvolid=0" "noatime" "nodiratime" "discard=async" ]; };

    ${config.services.ipfs.dataDir} = hdd // { options = [ "subvol=ipfs" ]; };

    "/boot" = {
      device = "/dev/disk/by-uuid/4305-4121";
      fsType = "vfat";
    };
  };
  systemd.services.srv-facl = {
    after = [ "srv.mount" ];
    script = "${pkgs.acl}/bin/setfacl -Rdm g:users:rwX /srv";
    wantedBy = [ "local-fs.target" ];
  };
  systemd.mounts = lib.mkForce [];

  swapDevices = [
   #{ device = "/dev/disk/by-uuid/86868083-921c-452a-bf78-ae18f26b78bf"; }
  ];

  virtualisation.libvirtd.enable = true;
  virtualisation.virtualbox.host.enable = false;
  virtualisation.anbox.enable = true;
  systemd.network.networks = {
    "40-anbox0".networkConfig.ConfigureWithoutCarrier = true;
  };

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  headless = false;

  nix = {
    gc.automatic = false; # We'll just use min-free instead
    gc.dates = "12:00"; # I'm now conditioned to be scared of midday
    gc.options = "--delete-older-than 8d";

    autoOptimiseStore = false; # Disabled for speed
    optimise.automatic = true;
    optimise.dates = [ "17:30" "02:00" ];

    maxJobs = 8;
    nrBuildUsers = 64;

    extraOptions = with usr.units; ''
      min-free = ${toString (gigabytes 48)}
    '';

    buildMachines =
     #(lib.optional true {
     #  hostName = "localhost";
     #  #system = "x86_64-linux";
     #  systems = ["x86_64-linux" "i686-linux" ] ++ config.boot.binfmt.emulatedSystems;
     #  inherit (config.nix) maxJobs;
     #  speedFactor = 4;
     #  supportedFeatures = config.nix.systemFeatures;
     #  mandatoryFeatures = [ ];
     #})++
      (lib.optional true {
        hostName = hosts.wireguard.ipv4.zeta;
        #sshUser = "nix-ssh";
        sshKey = "/etc/nix/id_zeta.ed25519";
        systems = ["x86_64-linux" "i686-linux" "armv6l-linux" "armv7l-linux"];
        maxJobs = 4;
        speedFactor = 2;
        supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
        mandatoryFeatures = [ ];
      }) ;
    distributedBuilds = true;
  };

  hardware.ckb-next.enable = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.cpu = {
    intel.updateMicrocode = true;
    amd.updateMicrocode = true;
  };

  programs.firejail = {
    enable = true;
    wrappedBinaries = builtins.mapAttrs (k: builtins.toPath) {
      firefox-safe-x11 = pkgs.writeScript "firefox" ''
        env MOZ_ENABLE_WAYLAND=1 ${lib.getBin pkgs.firefox}/bin/firefox
      '';
      firefox-safe-wl = pkgs.writeScript "firefox" ''
        env -u MOZ_ENABLE_WAYLAND ${lib.getBin pkgs.firefox}/bin/firefox
      '';
      chromium-safe = "${lib.getBin pkgs.chromium}/bin/chromium";
      teams-safe = "${lib.getBin pkgs.teams}/bin/teams"; # broken a.f.
      mpv-safe = "${lib.getBin pkgs.mpv}/bin/mpv"; # broken too, apparently
    };
  };
  programs.vim.defaultEditor = true;
  programs.adb.enable = true;
  programs.tmux.enable = true;
  programs.xonsh.enable = true;
  programs.singularity.enable = true;

  services.printing.enable = true;
  services.nix-index.enable = true;
  services.locate.enable = true;
  services.pcscd.enable = true;
  services.flatpak.enable = true;
  xdg.portal.enable = true;
  services.searx.enable = true;
  services.hydra.enable = true;
  services.gitfs = {
    enable = true;
    mounts = {
      nixrc = {
        directory = "/run/git/nixrc";
        remote = "/srv/git/github.com/bqv/nixrc";
        branch = "substrate";
      };
     #"/run/git/nixpkgs" = {
     #  github.owner = "nixos";
     #  github.repo = "nixpkgs";
     #};
    };
  };
  systemd.services.flake-ci = {
    enable = true;
    description = "Flake CI";
    path = [ pkgs.nixUnstable ];
    serviceConfig.Type = "oneshot";
    serviceConfig.User = config.users.users.bao.name;
    serviceConfig.WorkingDirectory = "/srv/git/github.com/bqv/nixrc";
    serviceConfig.ExecStart = "nix develop -c forecast master small";
  };
  systemd.timers.flake-ci = {
    enable = true;
    description = "Flake CI timer";
    timerConfig = {
      OnCalendar = "hourly";
      Unit = "flake-ci.service";
    };
    wantedBy = [ "timers.target" ];
  };

 #security.pam.loginLimits = [
 #  { domain = "@wheel"; item = "nofile"; type = "hard"; value = "unlimited"; }
 #  { domain = "@wheel"; item = "nofile"; type = "soft"; value = "1048576"; }
 #];

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIYNqRbITjMHmgD/UC87BISFTaw7Tq1jNd8X8i26x4b5 root@delta"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOvcvk1nLYImKqjhL8HdAb1sM2vXcEGu+rMZJ8XIG4H7 bao@delta"
  ];

  environment.systemPackages = with pkgs; [
    clipmenu bitwarden bitwarden-cli pass protonmail-bridge

    ckb-next element-desktop nheko discord ripcord
    brave vivaldi vivaldi-ffmpeg-codecs vivaldi-widevine
    qutebrowser firefox thunderbird electronmail mpv apvlv

    dunst catt termite rxvt_unicode
    steam obs-studio epsxe

    virt-manager
    anbox #pmbootstrap

    (with hunspellDicts; hunspellWithDicts [ en_GB-large ])
    wineWowPackages.staging

    giara lbry
    hnix

    mactelnet wold
  ];

  environment.etc."nix/id_zeta.ed25519".source = "${usr.secrets.keyDir}/nix/id_zeta.ed25519";
  environment.etc."nix/id_zeta.ed25519".mode = "0400";
  environment.etc."ssh/ssh_host_rsa_key".source = "${usr.secrets.keyDir}/deltassh/ssh_host_rsa_key";
  environment.etc."ssh/ssh_host_rsa_key".mode = "0400";
  environment.etc."ssh/ssh_host_rsa_key.pub".source = "${usr.secrets.keyDir}/deltassh/ssh_host_rsa_key.pub";
  environment.etc."ssh/ssh_host_ed25519_key".source = "${usr.secrets.keyDir}/deltassh/ssh_host_ed25519_key";
  environment.etc."ssh/ssh_host_ed25519_key".mode = "0400";
  environment.etc."ssh/ssh_host_ed25519_key.pub".source = "${usr.secrets.keyDir}/deltassh/ssh_host_ed25519_key.pub";
  environment.etc."ssh/ssh_host_dsa_key".source = "${usr.secrets.keyDir}/deltassh/ssh_host_dsa_key";
  environment.etc."ssh/ssh_host_dsa_key".mode = "0400";
  environment.etc."ssh/ssh_host_dsa_key-cert.pub".source = "${usr.secrets.keyDir}/deltassh/ssh_host_dsa_key-cert.pub";
  environment.etc."ssh/ssh_host_dsa_key.pub".source = "${usr.secrets.keyDir}/deltassh/ssh_host_dsa_key.pub";
  environment.etc."ssh/ssh_host_ecdsa_key".source = "${usr.secrets.keyDir}/deltassh/ssh_host_ecdsa_key";
  environment.etc."ssh/ssh_host_ecdsa_key".mode = "0400";
  environment.etc."ssh/ssh_host_ecdsa_key-cert.pub".source = "${usr.secrets.keyDir}/deltassh/ssh_host_ecdsa_key-cert.pub";
  environment.etc."ssh/ssh_host_ecdsa_key.pub".source = "${usr.secrets.keyDir}/deltassh/ssh_host_ecdsa_key.pub";
  environment.etc."ssh/ssh_host_ed25519_key-cert.pub".source = "${usr.secrets.keyDir}/deltassh/ssh_host_ed25519_key-cert.pub";
  environment.etc."ssh/ssh_host_rsa_key-cert.pub".source = "${usr.secrets.keyDir}/deltassh/ssh_host_rsa_key-cert.pub";
  environment.etc."ssh/ssh_revoked_keys".text = "";
  environment.etc."ssh/ssh_user-ca.pub".source = "${usr.secrets.keyDir}/deltassh/ssh_user-ca.pub";
  environment.etc."ssh/ssh_host-ca.pub".source = "${usr.secrets.keyDir}/deltassh/ssh_host-ca.pub";

  lib.test = let
    inherit (pkgs.withSources) processmgmt;
    exprFile = "${processmgmt}/examples/services-agnostic/processes.nix";
    s6Env = import "${processmgmt}/nixproc/backends/s6-rc/build-s6-rc-env.nix";
    svdir = s6Env {
      inherit exprFile;
      extraParams = {};
    };
    tools = import "${processmgmt}/tools" { inherit pkgs; };
    compdir = pkgs.runCommandLocal "s6-compiled" {} ''
      ${pkgs.s6-rc}/bin/s6-rc-compile -v 3 $out ${svdir}/etc/s6/sv
    '';
    init = pkgs.writeShellScript "s6-init" ''
      SCANDIR=/etc/s6
      export PATH=${with pkgs; lib.makeBinPath [
        coreutils shadow tools.s6-rc tools.common
        s6 s6-rc s6-linux-utils s6-portable-utils execline
        dysnomia glibc.bin findutils
      ]}:$PATH
      useradd -rUM s6-log
      useradd -rUM mongodb
      useradd -rUM influxdb
      useradd -rUM tomcat
      useradd -rUM httpd
      useradd -rUM mysql
      groupadd -r root

      s6-mkdir -p $SCANDIR /var/run
      s6-svscan $SCANDIR & # cheaper than s6-linux-init
      PID=$!
      nixproc-s6-rc-deploy ${svdir}
      ls /var/run
      wait $PID
    '';
    test = pkgs.writeShellScript "s6-test" ''
      doas systemd-nspawn --volatile=overlay --bind=/nix --bind=/run/current-system/ ${init}
    '';
  in {
    inherit exprFile svdir tools compdir init;
  } // pkgs.mkShell {
    buildInputs = [ test ];
  };
}
