{ config, lib, pkgs, ... }:

let inherit (lib) fileContents;
in {
  nix.package = pkgs.nixFlakes;

  nix.systemFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];

  imports = [
    ../local/locale.nix
  ];

  boot = {
    kernelPackages = pkgs.large.linuxPackages_latest;

    tmpOnTmpfs = true;
    cleanTmpDir = true;

    kernel.sysctl = {
      "kernel.sysrq" = 1;
      "kernel.printk" = "3 4 1 3";
      "net.ipv4.ip_forward" = "1";
    };
  };

  hardware.enableAllFirmware = true;
  nixpkgs.config.allowUnfree = true;
  zramSwap.enable = true;

  environment = {
    systemPackages = with pkgs; [
      binutils
      coreutils
      curl
      disnixos
      dosfstools
      dnsutils
      fd
      git
      gotop
      gptfdisk
      iputils
      manpages
      moreutils
      ripgrep
      stdmanpages
      utillinux
    ];
  };

  fonts = {
    fonts = with pkgs; [ powerline-fonts dejavu_fonts ];

    fontconfig.defaultFonts = {
      monospace = [ "DejaVu Sans Mono for Powerline" ];
      sansSerif = [ "DejaVu Sans" ];
    };
  };

  nix = {

    gc.automatic = lib.mkDefault true;
    autoOptimiseStore = lib.mkDefault false;
    optimise.automatic = lib.mkDefault true;

    maxJobs = lib.mkDefault 4;

    useSandbox = true;

    allowedUsers = [ "@wheel" ];

    trustedUsers = [ "root" "@wheel" ];

    extraOptions = ''
      experimental-features = nix-command flakes ca-references recursive-nix
    '';

    binaryCaches = [
      "https://hydra.nixos.org"
      "https://r-ryantm.cachix.org"
      "https://arm.cachix.org"
    ];

    binaryCachePublicKeys = [
      "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
      "r-ryantm.cachix.org-1:gkUbLkouDAyvBdpBX0JOdIiD2/DP1ldF3Z3Y6Gqcc4c="
      "arm.cachix.org-1:fGqEJIhp5zM7hxe/Dzt9l9Ene9SY27PUyx3hT9Vvei0="
    ];
  };

  virtualisation.virtualbox.host.package = pkgs.large.virtualbox;

  security = {
    apparmor = {
      enable = true;
    };

    hideProcessInformation = true;

    protectKernelImage = true;
  };

  services.earlyoom.enable = true;

  users.mutableUsers = false;

  home-manager.useUserPackages = true;
}
