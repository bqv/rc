args@{ nixpkgs, home, nur, self, config, lib, pkgs, system, ... }:

{
  imports = [
    ../legacy/delta/configuration.nix
    ../profiles/misc/disable-mitigations.nix
    ../profiles/networking/ipfs
    ../profiles/graphical/exwm
    ../users/root.nix
    ../users/bao.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot = {
      enable = true;
      configurationLimit = 8;
    };
  };

  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usbcore" "sd_mod" "sr_mod" "amdgpu" ];
  boot.initrd.kernelModules = [ "amdgpu" ];
  boot.kernelModules = [ "kvm-intel" "amdgpu" "fuse" ];
  boot.extraModulePackages = [ ];
  boot.binfmt.emulatedSystems = [ "armv7l-linux" "aarch64-linux" ];

  fileSystems = let
    btrfs = {
      device = "/dev/disk/by-uuid/f46f6fe4-c480-49f0-b3fb-22e61c57069c";
      fsType = "btrfs";
    };
  in {
    "/" = btrfs // { options = [ "subvol=nixos" ]; };
    "/home" = btrfs // { options = [ "subvol=home" ]; };
    "/nix" = btrfs // { options = [ "subvol=nix" ]; };
    "/games" = btrfs // { options = [ "subvol=games" ]; };
    "/var/run/btrfs" = btrfs // { options = [ "subvolid=0" ]; };

    "/boot" = {
      device = "/dev/disk/by-uuid/CEF4-EDD1";
      fsType = "vfat";
    };
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/86868083-921c-452a-bf78-ae18f26b78bf"; }
  ];

  virtualisation.libvirtd.enable = true;
  virtualisation.virtualbox.host.enable = true;
  virtualisation.anbox.enable = true;

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  hardware.bluetooth.enable = true;
  hardware.ckb-next.enable = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.cpu.intel.updateMicrocode = true;
  hardware.cpu.amd.updateMicrocode = true;

  programs.firejail = {
    enable = true;
    wrappedBinaries = {
      firefox-safe = "${lib.getBin pkgs.firefox}/bin/firefox";
      mpv-safe = "${lib.getBin pkgs.mpv}/bin/mpv";
    };
  };
  programs.vim.defaultEditor = true;
  programs.adb.enable = true;

  services.locate.enable = true;
  services.nixos-git = {
    enable = true;
    github = { owner = "bqv"; repo = "nixos"; };
    branch = "live";
    extraParams = {
      idle_fetch_timeout = 10;
    };
  };

  assertions = builtins.map (w: {
    assertion = false;
    message = w;
  }) config.warnings;
}
