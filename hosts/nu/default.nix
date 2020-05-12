{ config, pkgs, lib, domains, ... }:

{
  imports = [
  ];

  boot = {
    initrd = {
      availableKernelModules = [ "usbhid" ];
      kernelModules = [];
      checkJournalingFS = false;
    };
    kernelParams = [
      "console=ttyAMA0,115200"
      "console=tty1"
      "selinux=0"
      "plymouth.enable=0"
      "smsc95xx.turbo_mode=N"
      "dwc_otg.lpm_enable=0"
      "kgdboc=ttyAMA0,115200"
      "elevator=noop"
    ]; 
    kernelModules = [];
    kernelPackages = pkgs.linuxPackages_rpi2;
    consoleLogLevel = 5;
    loader = {
      initScript.enable = false;
      grub.enable = false;
      raspberryPi = {
        enable = true;
        uboot.enable = true;
        version = 2;
        firmwareConfig = ''
          gpu_mem=64
        '';
      };
      generic-extlinux-compatible.enable = false;
      generationsDir = {
        enable = false;
        copyKernels = true;
      };
    };
  };

  fileSystems = {
    "/" = {
      device = "/dev/mmcblk0p2";
      fsType = "f2fs";
    };
    "/boot" = {
      device = "/dev/mmcblk0p1";
      fsType = "vfat";
      neededForBoot = true;
    };
  };

  swapDevices = [ { device = "/dev/mmcblk0p3"; } ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";

  nix.maxJobs = lib.mkDefault 4;
  nix.buildMachines = [ {
    hostName = "nix-zeta";
    systems = ["x86_64-linux" "armv6l-linux" "armv7l-linux"];
    maxJobs = 3;
    speedFactor = 2;
    supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
    mandatoryFeatures = [ ];
  } ];
  nix.distributedBuilds = true;
  nix.extraOptions = ''
    builders-use-substitutes = true
  '';

  nixpkgs.config = {
    platform = pkgs.platforms.raspberrypi2;
  };

  networking.hostName = "nu";

  networking.useDHCP = false;
  networking.interfaces.eth0.useDHCP = true;

  console = {
    font = "Lat2-Terminus16";
    keyMap = "uk";
  };

  i18n = {
    defaultLocale = "en_GB.UTF-8";
  };

  time.timeZone = "Europe/London";

  environment.systemPackages = with pkgs; [
    wget htop screen pv git dnsutils
    (callPackage home-assistant-cli.override {
      python3 = python3 // {
        pkgs = python3.pkgs // {
          aiohttp = python3.pkgs.aiohttp.overrideAttrs (oldAttrs: {
            doInstallCheck = false;
          });
        };
      };
    })
    dvtm abduco
    (arm-adb.override {
     #boringssl = boringssl.override { 
     #  go = pkgs.go.override { 
     #  };
     #};
    })
  ];

  programs.vim.defaultEditor = true;
  programs.mosh.enable = true;
  programs.tmux.enable = true;

  services.resolved.enable = true;
  services.resolved.dnssec = "false";
  services.openssh.enable = true;
  services.openssh.permitRootLogin = "without-password";
  services.avahi.enable = true;
  services.avahi.ipv4 = true;
  services.avahi.ipv6 = true;
  services.avahi.nssmdns = true;
  services.avahi.allowPointToPoint = true;
  services.avahi.publish.enable = true;
  services.avahi.publish.hinfo = true;
  services.avahi.publish.userServices = true;
  services.avahi.publish.addresses = true;
  services.avahi.publish.domain = true;
  services.mosquitto.enable = true;
  services.mosquitto.allowAnonymous = true;
  services.mosquitto.users = {};

  services.home-assistant.enable = true;
  services.home-assistant.config = {
    homeassistant = {
      latitude = 53.959860;
      longitude = -1.051170;
      elevation = 12;
      unit_system = "metric";
      time_zone = "Europe/London";
      name = "Home";
      whitelist_external_dirs = [ "/tmp" ];
    };
    camera = {
      platform = "ffmpeg";
      name = "Camera";
      input = "/dev/video0";
    };
    http = {
      base_url = "https://home.${domains.home}:80";
    };
    media_player = {
      platform = "androidtv";
      name = "Omega";
      host = "192.168.0.128";
      adb_server_ip = "127.0.0.1";
      apps = {
        com.google.android.leanbacklauncher = "Home Screen";
        com.google.android.apps.mediashell = "Chromecast";
        com.amazon.amazonvideo.livingroom = "Amazon Prime Video";
      };
    };
  };
  services.home-assistant.package = pkgs.home-assistant.override {
    extraPackages = py: with pkgs; [
      ffmpeg
      picotts
      #py.adb-homeassistant
    ];
    packageOverrides = self: super: {
      aiohttp = super.aiohttp.overrideAttrs (oldAttrs: {
        doInstallCheck = false;
        #nativeBuildInputs = [ pkgs.pythonPackages.adb-homeassistant ];
      });
    };
  };
  # read /etc/hass/configuration.yaml

  networking.firewall.enable = false;

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = false;

  sound.enable = true;
  # hardware.pulseaudio.enable = true;

  users.users.leaf = {
    #uid = 1001;
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?
}
