{ config, lib, pkgs, ... }:

let
  inherit (lib) fileContents;
in {
  nix.package = pkgs.nix;

  nix.systemFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];

  boot = {
    kernelPackages = pkgs.large.linuxPackages_latest;

    tmpOnTmpfs = true;
    cleanTmpDir = true;

    kernel.sysctl = {
      "kernel.sysrq" = 1;
      "kernel.printk" = "3 4 1 3";
      "net.ipv4.ip_forward" = "1";
      "kernel.pid_max" = "4194304";
    };

    extraModprobeConfig = lib.mkIf config.networking.nftables.enable ''
      install ip_tables ${pkgs.coreutils}/bin/true
    '';
  };

  hardware.enableAllFirmware = true;
  nixpkgs.config.allowUnfree = true;
  zramSwap.enable = true;

  console.useXkbConfig = true;

  i18n.defaultLocale = "en_GB.UTF-8";

  time.timeZone = "Europe/London";

  environment = let
    nixos-rebuild = pkgs.callPackage ../pkgs/lib/nixos-rebuild.nix {
      nix = config.nix.package;
      nixos-rebuild = config.system.build.nixos-rebuild;
    };
  in {
    systemPackages = with pkgs; [
      binutils
      coreutils
      curl
      disnixos
      (pkgs.lowPrio dosfstools)
      dnsutils
      fd
      git
      gotop
      gptfdisk
      iputils
      manpages
      moreutils
      (pkgs.hiPrio nixos-rebuild)
      perl
      ripgrep
      rsync
      stdmanpages
      utillinux
    ];
    enableDebugInfo = true;
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

    useSandbox = "relaxed";

    allowedUsers = [ "@wheel" ];

    trustedUsers = [ "root" "@wheel" ];

    extraOptions = ''
      show-trace = true
      builders-use-substitutes = true
      experimental-features = nix-command flakes ca-references recursive-nix
    '';

    binaryCaches = [
      "https://nixpkgs-wayland.cachix.org"
      "https://nix-community.cachix.org"
      "https://cross-armed.cachix.org"
      "https://r-ryantm.cachix.org"
      "https://all-hies.cachix.org"
      "https://iohk.cachix.org"
      "https://arm.cachix.org"
    ];

    binaryCachePublicKeys = [
      "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "cross-armed.cachix.org-1:v+RBneV2nKPSKRe3/qUFhOG6/9GE+o0lw9/NW/wX9Hk="
      "r-ryantm.cachix.org-1:gkUbLkouDAyvBdpBX0JOdIiD2/DP1ldF3Z3Y6Gqcc4c="
      "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
      "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
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
