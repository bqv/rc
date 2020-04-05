{ config, lib, pkgs, ... }:

{
  imports =
    [
      ../../legacy/zeta/configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot = {
    enable = true;
    configurationLimit = 8;
  };
  boot.loader.efi.canTouchEfiVariables = true;
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

  swapDevices = [ ];

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
