{ config, lib, pkgs, ... }:
let inherit (lib) fileContents;

in {
  nix.package = pkgs.nixFlakes;

  nix.systemFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];

  imports = [
    ../local/locale.nix
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;

    tmpOnTmpfs = true;
    cleanTmpDir = true;

    kernel.sysctl = {
      "kernel.sysrq" = 1;
      "kernel.printk" = "3 4 1 3";
    };
  };

  hardware.enableAllFirmware = true;
  nixpkgs.config.allowUnfree = true;

  system.extraSystemBuilderCmds = ''
    ln -s '${../configuration.nix}' \
      "$out/configuration.nix"
  '';

  environment = {
    systemPackages = with pkgs; [
      binutils
      coreutils
      curl
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

    gc.automatic = true;
    gc.dates = "12:00";
    gc.options = "--delete-older-than 8d";

    autoOptimiseStore = false; # Disabled for speed
    optimise.automatic = true;
    optimise.dates = [ "17:30" "02:00" ];

    maxJobs = lib.mkDefault 4;

    useSandbox = true;

    allowedUsers = [ "@wheel" ];

    trustedUsers = [ "root" "@wheel" ];

    extraOptions = ''
      experimental-features = nix-command flakes ca-references
    '';
  };

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
