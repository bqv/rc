{ nixpkgs, home, nur, self, config, lib, pkgs, system, ... }:

{
  imports = [
    ../../profiles/meta/fatal-warnings.nix
    ../../profiles/misc/disable-mitigations.nix
    ../../profiles/misc/udev-nosettle.nix
    ../../profiles/misc/guix.nix
    ../../profiles/networking/ipfs
    ../../profiles/networking/bluetooth
    ../../profiles/networking/wireguard
    ../../profiles/sound/pulse.nix
    ../../profiles/graphical/exwm
    ../../profiles/weechat.nix
    ../../users/root.nix
    ../../users/bao.nix
    ./xserver.nix
    ./network.nix
    ./remote.nix
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
  virtualisation.virtualbox.host.enable = false;
  virtualisation.anbox.enable = true;

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  nix = {
    gc.automatic = true;
    gc.dates = "12:00";
    gc.options = "--delete-older-than 8d";

    autoOptimiseStore = false; # Disabled for speed
    optimise.automatic = true;
    optimise.dates = [ "17:30" "02:00" ];
  };

  hardware.ckb-next.enable = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.cpu = {
    intel.updateMicrocode = true;
    amd.updateMicrocode = true;
  };

  programs.firejail = {
    enable = true;
    wrappedBinaries = {
      chromium-safe = "${lib.getBin pkgs.large.chromium}/bin/chromium";
      firefox-safe = "${lib.getBin pkgs.large.firefox}/bin/firefox";
      mpv-safe = "${lib.getBin pkgs.mpv}/bin/mpv";
    };
  };
  programs.vim.defaultEditor = true;
  programs.adb.enable = true;
  programs.tmux.enable = true;

  services.disnix.enable = true;
  services.printing.enable = true;
  services.nix-index.enable = true;
  services.locate.enable = true;
  services.guix.enable = true;
  services.guix.package = with pkgs.pr 84004
    "sha256-jg8TNShjvt+09LYJIHRiFsHC25Di7khqmtGnY+GwUBo="; guix;
  services.nixos-git = {
    enable = true;
    github = { owner = "bqv"; repo = "nixos"; };
    branch = "live";
    extraParams = {
      idle_fetch_timeout = 10;
    };
  };

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = false;

  nix.buildMachines = lib.optional false [
    {
      hostName = "10.0.0.1";
      sshUser = "nix-ssh";
      sshKey = "/root/.ssh/nix_remote";
      #system = "x86_64-linux";
      systems = ["x86_64-linux" "i686-linux" "armv6l-linux" "armv7l-linux"];
      maxJobs = 4;
      speedFactor = 4;
      supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
      mandatoryFeatures = [ ];
    }
  ];
  nix.distributedBuilds = true;
  nix.extraOptions = ''
    builders-use-substitutes = true
  '';

  environment.systemPackages = with pkgs.large; [
    clipmenu bitwarden bitwarden-cli pass protonmail-bridge

    ckb-next riot-desktop nheko discord ripcord
    qutebrowser next firefox fx_cast_bridge
    thunderbird mpv

    dunst catt termite rxvt_unicode
    steam obs-studio

    anbox #pmbootstrap

    (with hunspellDicts; hunspellWithDicts [ en_GB-large ])
  ];
  nixpkgs.config.firefox.enableFXCastBridge = true;
}
