args@{ nixpkgs, home, nur, self, config, lib, pkgs, system, ... }:

{
  imports = [
    ../../legacy/delta/graphical
    ../../legacy/delta/network/tinc.nix
    ../../profiles/meta/fatal-warnings.nix
    ../../profiles/misc/disable-mitigations.nix
    ../../profiles/misc/udev-nosettle.nix
    ../../profiles/misc/guix.nix
    ../../profiles/networking/ipfs
    ../../profiles/networking/bluetooth
    ../../profiles/sound/pulse.nix
    ../../profiles/graphical/exwm
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
  virtualisation.virtualbox.host.enable = true;
  virtualisation.anbox.enable = true;

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  hardware.ckb-next.enable = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.cpu = {
    intel.updateMicrocode = true;
    amd.updateMicrocode = true;
  };

  programs.firejail = {
    enable = true;
    wrappedBinaries = {
      firefox-safe = "${lib.getBin pkgs.large.firefox}/bin/firefox";
      mpv-safe = "${lib.getBin pkgs.mpv}/bin/mpv";
    };
  };
  programs.vim.defaultEditor = true;
  programs.adb.enable = true;
  programs.tmux.enable = true;

  services.printing.enable = true;
  services.nix-index.enable = true;
  services.locate.enable = true;
  services.guix.enable = false;
  services.guix.package = with pkgs.pr 84004 "sha256-JmRN8WydXfoiqcANKjgd/lAQZAtscT1QOc2KcznwFxM="; let
    guile-gnutls = (gnutls.override {
      inherit guile;
      guileBindings = true;
    }).overrideAttrs (attrs: {
      configureFlags = [
        "--with-guile-site-dir=\${out}/share/guile/site"
        "--with-guile-site-ccache-dir=\${out}/share/guile/site"
        "--with-guile-extension-dir=\${out}/share/guile/site"
      ];
    });
  in guix.overrideAttrs (super: rec {
    preAutoreconf = ''
      sed -i '/exec autoreconf/d' ./bootstrap && ./bootstrap
    '';
    src = fetchFromSavannah {
      repo = "guix";
      rev = "b256d136199b6e2a53ee547b9239e689697c017f";
      sha256 = "1vk3jkbg76plld905d7anggsgpv2x29q6c3avpaaq1hcl0s3knwp";
    };
    nativeBuildInputs = super.nativeBuildInputs
      ++ [ autoreconfHook texinfo ];
    GUILE_LOAD_PATH = lib.concatStringsSep ":" [
      "${guile-gcrypt}/share/guile/site/2.2"
      "${guile-git}/share/guile/site/2.2"
      "${guile-json}/share/guile/site"
      "${guile-sqlite3}/share/guile/site/2.2"
      "${guile-ssh}/share/guile/site/2.2"
      "${guile-gnutls.out}/share/guile/site"
      "${guile-git.bytestructures}/share/guile/site/2.2"
    ];
    GUILE_LOAD_COMPILED_PATH = GUILE_LOAD_PATH;
  });
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
}
