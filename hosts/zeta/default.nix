{ config, lib, pkgs, ... }:

{
  imports = [
    ../../legacy/services/http.nix
    ../../legacy/services/sync.nix
    ../../legacy/services/mastodon.nix
    ../../legacy/services/minecraft.nix
    ../../legacy/containers/secure.nix
    ../../legacy/containers/sandbox.nix
   #../../legacy/containers/certmon.nix
    ../../legacy/containers/authority.nix
    ../../legacy/containers/search.nix
    ../../legacy/containers/mastodon.nix
    ../../legacy/containers/matrix.nix
    ../../profiles/meta/fatal-warnings.nix
    ../../profiles/misc/qemu.nix
    ../../profiles/security/sudo.nix
    ../../profiles/networking/wireguard
    ../../profiles/networking/tor
    ../../profiles/services/syncthing
    ../../users/root.nix
    ../../users/bao.nix
    ./network.nix
    ./certificate.nix
  ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ "btrfs" "ext4" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  boot.binfmt.emulatedSystems = [ "armv7l-linux" "aarch64-linux" ];

  boot.cleanTmpDir = true;

  boot.kernel.sysctl = {
    "net.core.somaxconn" = 512;
    "vm.overcommit_memory" = 1;
  };

  boot.kernelParams = [
    "transparent_hugepage=never"
  ];

  environment.etc.limits = {
    target = "security/limits.conf";
    text = ''
      # add following lines to it
      *    soft     nproc          65535    
      *    hard     nproc          65535   
      *    soft     nofile         65535   
      *    hard     nofile         65535
      root soft     nproc          65535    
      root hard     nproc          65535   
      root soft     nofile         65535   
      root hard     nofile         65535
    '';
  };

  qemu-user.arm = true;

  fileSystems."/" =
    { device = "/dev/sda3";
      fsType = "btrfs";
      options = [ "subvol=nixos" ];
    };

  fileSystems."/nix/.rw-store" =
    { device = "tmpfs";
      fsType = "tmpfs";
    };

  fileSystems."/boot" =
    { device = "/dev/sda2";
      fsType = "ext4";
      options = [ "rw" "data=ordered" ];
    };

  boot.tmpOnTmpfs = true;
  boot.loader = {
    grub = {
      enable = true;
      efiSupport = false;
      device = "nodev";
    };
  };

  fileSystems."/home" =
    { device = "/dev/sda3";
      fsType = "btrfs";
      options = [ "subvol=home" ];
    };

  fileSystems."/srv" =
    { device = "/dev/sda3";
      fsType = "btrfs";
      options = [ "subvol=srv" ];
    };

  fileSystems."/var/run/btrfs" =
    { device = "/dev/sda3";
      fsType = "btrfs";
      options = [ "subvolid=0" ];
    };

  fileSystems."/var/lib/machines/sandbox" =
    { device = "/dev/sda3";
      fsType = "btrfs";
      options = [ "subvol=arch" ];
    };

  swapDevices = [ { device = "/dev/sda4"; } ];

  virtualisation.libvirtd.enable = false;
  virtualisation.virtualbox.host.enable = false;

  nix.gc.automatic = true;
  nix.gc.dates = "05:00";
  nix.gc.options = "";
  nix.autoOptimiseStore = true;
  nix.optimise.automatic = true;
  nix.optimise.dates = [ "12:30" "00:30" ];
  nix.maxJobs = 8;
  #powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  services.disnix.enable = true;

  # Until further migration.
  users.mutableUsers = lib.mkForce true;

  ## Migrated
  services.avahi.enable = true;
  services.avahi.nssmdns = true;
  services.avahi.allowPointToPoint = true;
  services.avahi.publish.enable = true;
  services.avahi.publish.addresses = true;
  services.postgresql.enable = true;
  services.postgresql.enableTCPIP = true;
  services.postgresql.extraConfig = ''
    work_mem = 16MB
  '';
  services.postgresql.authentication = lib.mkForce
    (let
      mastodon = config.services.mastodon.database;
     in ''
      # Generated file; do not edit!
      local all all              ident
      host  all all 127.0.0.1/32 md5
      host  all all ::1/128      md5
      host  ${mastodon.name} ${mastodon.user} 10.6.0.0/24 trust
      host  matrix-synapse matrix-synapse 10.7.0.0/24 trust
      host  mx-puppet-discord mx-puppet-discord 10.7.0.0/24 trust
      host  lemmy lemmy 10.0.0.3/32 trust
     '');
  services.postgresql.ensureUsers = [
    {
      name = "lemmy";
      ensurePermissions."DATABASE \"lemmy\"" = "ALL PRIVILEGES";
    }
  ];
  services.postgresql.ensureDatabases = [ "lemmy" ];
  services.openssh.enable = true;
  services.openssh.forwardX11 = true;
  services.openssh.allowSFTP = true;
  services.openssh.openFirewall = false;
  services.openssh.permitRootLogin = "without-password";
  services.openssh.listenAddresses = [
    { addr = "127.0.0.1"; port = 22; }
    { addr = "10.0.0.1"; port = 22; }
    { addr = "10.1.0.1"; port = 22; }
  ];
  services.openssh.passwordAuthentication = false;
  services.openssh.challengeResponseAuthentication = true;
  services.openssh.hostKeys = [
    { openSSHFormat = true; path = "/etc/ssh/ssh_host_dsa_key"; type = "dsa"; bits = 1024; }
    { openSSHFormat = true; path = "/etc/ssh/ssh_host_rsa_key"; type = "rsa"; bits = 4096; }
    { openSSHFormat = true; path = "/etc/ssh/ssh_host_ecdsa_key"; type = "ecdsa"; bits = 521; }
    { openSSHFormat = true; path = "/etc/ssh/ssh_host_ed25519_key"; type = "ed25519"; }
  ];
  services.openssh.extraConfig =
    let
      certificates = map (cert: "HostCertificate ${cert}") [
        "/etc/ssh/ssh_host_dsa_key-cert.pub"
        "/etc/ssh/ssh_host_rsa_key-cert.pub"
        "/etc/ssh/ssh_host_ecdsa_key-cert.pub"
        "/etc/ssh/ssh_host_ed25519_key-cert.pub"
      ];
      principals = "AuthorizedPrincipalsFile %h/.ssh/authorized_principals";
      userkeys = "TrustedUserCAKeys /etc/ssh/ssh_user-ca.pub";
      revokedkeys = "RevokedKeys /etc/ssh/ssh_revoked_keys";
    in builtins.concatStringsSep "\n" (certificates ++ [ userkeys revokedkeys ]);

  programs.mosh.enable = true;
  environment.variables.MOSH_SERVER_NETWORK_TMOUT = "86400";
  programs.ssh = let
    algorithms = [
      "ssh-dss" "ssh-dss-cert-v01@openssh.com"
      "ssh-rsa" "ssh-rsa-cert-v01@openssh.com"
      "rsa-sha2-256" "rsa-sha2-512" "rsa-sha2-256-cert-v01@openssh.com" "rsa-sha2-512-cert-v01@openssh.com"
      "ecdsa-sha2-nistp256" "ecdsa-sha2-nistp384" "ecdsa-sha2-nistp521"
      "ecdsa-sha2-nistp256-cert-v01@openssh.com" "ecdsa-sha2-nistp384-cert-v01@openssh.com" "ecdsa-sha2-nistp521-cert-v01@openssh.com"
      "sk-ecdsa-sha2-nistp256@openssh.com" "sk-ecdsa-sha2-nistp256-cert-v01@openssh.com"
      "ssh-ed25519" "ssh-ed25519-cert-v01@openssh.com"
      "sk-ssh-ed25519@openssh.com" "sk-ssh-ed25519-cert-v01@openssh.com"
    ];
  in {
    forwardX11 = true;
    setXAuthLocation = true;
    pubkeyAcceptedKeyTypes = algorithms;
    hostKeyAlgorithms = algorithms;
    extraConfig = ''
      VisualHostKey yes
    '';
    knownHosts = {
      "Alpha CA" = {
        certAuthority = true;
        hostNames = [ "*" ];
        publicKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDC1RTc/vJo3hBwXnLYnXbWpZseiuuqh0WqASObA1yw5NuoEtrTyQp2wiDOp1kfZw0iqYJ5slMg5ddvVzOky85B7AaBEiD6R+Bx8V9FFpCfMVoKvXd1O4PgQVD0Rjc7WHdOQY+z7mfWA7GLvwVL31EwfqeoEc8GaO0aRMFIGkfVVToT3g2d1ROwjT8aZJmQUJoVvNHQX0HWxWsAUryTKS/1s/1WWCB9w5+Eynpr1RU6D/bXRoNQZHnDkBY93R1PudxiyAymVdBfmdtO3fprTGqmCV+d/9NnuwPYyIZ+LJBHQEIjoz9osQABSl9HQaUxoPmqWCO3iePsiJ7rNDut+zhke2Ch48ynOFYnYfj9CwAJTR7tGoQBIP5HBsdZT35aAw1M9iMfrJGm9dzVjudrkHgEmGferId6vr/F0pla7S2gsh5eAXCILNoZ2jsROIsVTL4rwtOWSI71q26CCCbu7kylVZY8KIXHA0ndjaq6sc2bnNUmt0rQnqbjUxCXhRL3wCn/F7PueZfMq5v5PS7alGvOYe+enqcCJqbibRcXx70R54KXwGCcSgrKAGh1HAmrTqFKsc/2JiW8rr2/Lzzse/Mj+K3BuOU6J1C0bWd5yaH2VpOTvLZITPJHmHedN8t5/SzPZzVJaaRT7LJk2B3nLRhVCp8Vg7PZqf5KJggkx6p7Mw== Alpha CA";
      };
    };
  };

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = false;

  nix.sshServe.enable = true;
  nix.sshServe.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIYNqRbITjMHmgD/UC87BISFTaw7Tq1jNd8X8i26x4b5 root@delta"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOvcvk1nLYImKqjhL8HdAb1sM2vXcEGu+rMZJ8XIG4H7 bao@delta"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEEI6mOJHEH+bbho1V/8dBCdAwORp7zrWoMyue8hBllU root@nu"
  ];

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIYNqRbITjMHmgD/UC87BISFTaw7Tq1jNd8X8i26x4b5 root@delta"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOvcvk1nLYImKqjhL8HdAb1sM2vXcEGu+rMZJ8XIG4H7 bao@delta"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEEI6mOJHEH+bbho1V/8dBCdAwORp7zrWoMyue8hBllU root@nu"
  ];
}
