{ config, lib, pkgs, ... }:

let
  inherit (lib) fileContents;
in {
  nix.systemFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];

  boot = {
    kernelPackages = lib.mkDefault (pkgs.large.linuxPackages_latest.extend (_: _: {
      anbox = pkgs.large.linuxPackages_latest.virtualboxGuestAdditions; # harmless
    }));

    tmpOnTmpfs = true;
    cleanTmpDir = true;

    kernel.sysctl = {
      "kernel.sysrq" = 1;
      "kernel.printk" = "3 4 1 3";
      "net.ipv4.ip_forward" = "1";
      "kernel.pid_max" = "4194304";
    };
  };
  systemd.mounts = [{
    where = "/tmp";
    what = "tmpfs";

    # 400k is unacceptably low for my nix builds
    options = "mode=1777,strictatime,nosuid,nodev,size=16G,nr_inodes=8M";
  }];

  hardware.enableAllFirmware = true;
  nixpkgs.config.allowUnfree = true;
  zramSwap.enable = true;

  console.useXkbConfig = true;

  i18n.defaultLocale = "en_GB.UTF-8";

  time.timeZone = "Europe/London";

  environment = {
    systemPackages = with pkgs; [
      binutils
      coreutils
      curl
      darcs
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

    allowedUsers = [ "@wheel" "hydra" ];

    trustedUsers = [ "root" "@wheel" ];

    extraOptions = ''
      show-trace = true
      builders-use-substitutes = true
      experimental-features = nix-command flakes ca-references recursive-nix
      preallocate-contents = true
      print-build-logs = true
      access-tokens = "github.com=${(import ../secrets/git.github.nix).oauth-token}"
    '';

    binaryCaches = [
      "https://static-haskell-nix.cachix.org"
      "https://nixpkgs-wayland.cachix.org"
      "https://nix-community.cachix.org"
      "https://cross-armed.cachix.org"
      "https://r-ryantm.cachix.org"
      "https://all-hies.cachix.org"
      "https://iohk.cachix.org"
      "https://arm.cachix.org"
    ];

    binaryCachePublicKeys = [
      "static-haskell-nix.cachix.org-1:Q17HawmAwaM1/BfIxaEDKAxwTOyRVhPG5Ji9K3+FvUU="
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
    audit.enable = true;
    auditd.enable = true;

    hideProcessInformation = true;

    protectKernelImage = true;
  };

  services.earlyoom.enable = true;

  documentation.nixos.includeAllModules = true;
  documentation.man.generateCaches = lib.mkForce true;

  users.mutableUsers = false;
}
