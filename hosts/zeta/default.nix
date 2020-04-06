{ config, lib, pkgs, ... }:

{
  imports = [
    ../../legacy/zeta/configuration.nix
    ../../profiles/networking/wireguard
  ];

  boot.cleanTmpDir = true;

  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ "btrfs" "ext4" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  boot.binfmt.emulatedSystems = [ "armv7l-linux" "aarch64-linux" ];

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

  virtualisation.libvirtd.enable = true;
  virtualisation.virtualbox.host.enable = true;

  nix.gc.automatic = true;
  nix.gc.dates = "05:00";
  nix.gc.options = "";
  nix.autoOptimiseStore = true;
  nix.optimise.automatic = true;
  nix.optimise.dates = [ "12:30" "00:30" ];
  nix.maxJobs = 8;
  #powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  # Enable bluetooth modules.
  hardware.bluetooth.enable = true;

  # Set your locale.
  i18n.defaultLocale = "en_GB.UTF-8";

  # Set your time zone.
  time.timeZone = "Europe/London";

  # Until further migration.
  users.mutableUsers = lib.mkForce true;
}
