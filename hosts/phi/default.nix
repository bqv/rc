{ config, pkgs, lib, domains, ... }:

{
  imports = [
   #../../profiles/meta/fatal-warnings.nix
    ../../profiles/misc/disable-mitigations.nix
    ../../profiles/misc/adblocking.nix
    ../../profiles/security/sudo.nix
    ../../profiles/networking/ipfs
    ../../profiles/networking/wireguard
    ../../profiles/networking/mdns.nix
    ../../profiles/sound/pulse.nix
    ../../users/leaf.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot = {
      enable = true;
      configurationLimit = 16;
    };
  };

  boot.extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback ];

  boot.initrd.availableKernelModules = [ "ata_generic" "ehci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" "sr_mod" ];
  boot.kernelModules = [ "kvm-intel" ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/7f4d3be4-6f2e-4f50-9bbe-493557965337";
      fsType = "btrfs";
      options = [ "subvol=os" ];
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/7f4d3be4-6f2e-4f50-9bbe-493557965337";
      fsType = "btrfs";
      options = [ "subvol=home" ];
    };

  fileSystems."/nix" =
    { device = "/dev/disk/by-uuid/7f4d3be4-6f2e-4f50-9bbe-493557965337";
      fsType = "btrfs";
      options = [ "subvol=nix" ];
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/CF89-F076";
      fsType = "vfat";
    };

  swapDevices = [ ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";

  nix.package = lib.mkForce pkgs.nix-ipfs;

  nix.maxJobs = lib.mkDefault 4;
  nix.buildMachines = [ {
    hostName = "nix-zeta";
    systems = [ "x86_64-linux" "aarch64-linux" "armv6l-linux" "armv7l-linux" ];
    maxJobs = 3;
    speedFactor = 2;
    supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
    mandatoryFeatures = [ ];
  } ];
  nix.distributedBuilds = true;
  nix.extraOptions = ''
    builders-use-substitutes = true
  '';

  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;

  environment.systemPackages = with pkgs; [
    wget htop screen pv git dnsutils direnv
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
  ];

  programs.vim.defaultEditor = true;
  programs.mosh.enable = true;
  programs.tmux.enable = true;
  programs.adb.enable = true;
  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "gnome3";
  };

  services.lorri.enable = true;
  services.resolved.enable = true;
  services.resolved.dnssec = "false";
  services.openssh.enable = true;
  services.openssh.permitRootLogin = "without-password";
  services.avahi.publish.hinfo = true;
  services.avahi.publish.userServices = true;
  services.avahi.publish.domain = true;
  services.mosquitto.enable = true;
  services.mosquitto.allowAnonymous = true;
  services.mosquitto.users = {};

  services.home-assistant.enable = true;
  services.home-assistant.autoExtraComponents = false; # this is so broken :|
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
      base_url = "https://home.${domains.home}:443";
    };
    media_player = [{
      platform = "androidtv";
      name = "Omega";
      host = "192.168.0.128";
      adb_server_ip = "127.0.0.1";
      apps = {
        com.google.android.leanbacklauncher = "Home Screen";
        com.google.android.apps.mediashell = "Chromecast";
        com.amazon.amazonvideo.livingroom = "Amazon Prime Video";
      };
    } {
      platform = "cast";
      host = "192.168.0.128";
    }];
    notify = [{
      platform = "nfandroidtv";
      name = "AndroidTV";
      host = "192.168.0.128";
    }];
    tuya = import ../../secrets/hass.tuya.nix;
  };
  services.home-assistant.package = pkgs.home-assistant.override {
    extraComponents = [
      "homeassistant"
      "frontend"
      "http"
      "camera"
      "ffmpeg"
      "ffmpeg_motion"
      "ffmpeg_noise"
      "cast"
      "media_player"
      "notify"
      "nfandroidtv"
      "tuya"
    ];
    extraPackages = py: [
      pkgs.ffmpeg
      py.ha-ffmpeg
      pkgs.picotts
      py.adb-homeassistant
      py.pip
      py.pyotp
    ];
  };
  # read /etc/hass/configuration.yaml

  systemd.services.adb = rec {
    description = "Android Debug Server Daemon";
    serviceConfig.Type = "forking";
    serviceConfig.ExecStart = "${pkgs.androidsdk_9_0}/bin/adb start-server";
    serviceConfig.ExecStop = "${pkgs.androidsdk_9_0}/bin/adb kill-server";
    wantedBy = [ "default.target" "home-assistant.target" ];
  };

  networking.firewall.enable = false;

  services.xserver.enable = true;
  services.xserver.layout = "gb";
  services.xserver.libinput.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;

  services.ipfs-cluster.enable = lib.mkForce false; # unprimed

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAAEAQC/Ca5MUwmHMZt4sH/xLAq20X12xDj2bSqg88thxWLvpqnLpLEJmpasaHKRRh9O6E/NE0Zsn45dsTuvB9/otoDGdstYX2VZyi3UBIWp3BjxdbvmMrbhvpihiZl6rAFXU45/LM/4PvraLic2ZXuOGFqoLcnKDAvCATJsQfUGcEnM0YJVcFLjA4sSPHarSoSHFxZ90RXVmW8m5sfNCsMMo3jCMgE49zlWhcrKfp/FfpIlKMytTDdb+JE0JIsR+28oxJtL63wTbO9p2zFS8geNPVMfj8+Ge4bb0YWroBNGaMo3cHcdtquWWvTzZxWJu+AiSlZIWcP6aOhOoRiFrCWIYBP91dtARm2+OXIpUNAT+yPR+YQ4BAUNxYyBZO3hTH47cq2dC4NAQVcjuUb5VLoRAOAl+DzHSj7FNFGo49CxE/eDlhoXzmgP28N5tDktZYYstrr4j/KAZPYzGW8tB6s/VhhiCGFgZQXq2irH2UtU3Amr7cw0Wiiuyoe4x67dUB+sy3yT5jle/dLe0U9qgAiFTptix1QpjDaT8dFYQsi0kzXlSUEB05pmnCjM2n9eksdtF5kggLDNQ3VKH+LRN/rd5JKJsMJn9Lxr9nO3x+x+fhBurthumrDB2S3oqEOWQDxJ+JaZi5mshzZ2bB4lSXGB5aYjaBSk49EmlrNo8dUUXpZFT9mePx8BIC73XtoEsFgH9kpvHqHxOaBBO3wsxWekbS5bzi2KkBWxMAHq/fVV37iNaDHnldEn9DGDIQb6fQnjLkWbwNRBsdZxqfJGBqVpUNVbsS4BOFoa7yxnCZ6OgxzXpScoyeTxgM1nDALRnwee2d+3GrKHzE23Db6tIwUUeHEkcNwY2L6MW6Z2Onv2T9+V0IITe16EV+TcTc04DQ2XtnVEqUfqK5NZX0wpDuO0Bw0cJrbxJy3lk1PbnUnP/slfRgB4yOvkA0zRrer27EiyRsQe9QSfcmcpIK66+UncYqTFZ/qJFBdupn2ruYaDfCq/G8HyNsm7fXJLgsnGrAfEqUlBabbmLBdSvPqWpkMCjmmX7tMvTTWV5/IdFo0NVPQ7VBdZrRoPbYOQetZqr6S1tkKvpISrBKMRXXgaCcegnqWKqomZsFWcTnhyV0vCJM16IaNp++2rqw426hvpm2F+hhUzDa7CP/+HKvo0ucwC1u24CX75eT/pEP7WuINNMEx1y+/VxyeWscAK89NYa2YGBzqBEJ9+0QVFNYye/baaNn1dNEuPk49bkM7g3LTp8ae+J+5dAb3QsKMxL/bssFzAc+7M5+LFQDFpWtxkzkB5+X9HzMtVbsgDk9CDgn56OfkpzjFR7PeDUcKeFWeccWHRMOyXVX1daoUTeDIpESID2FnR bao@delta"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOvcvk1nLYImKqjhL8HdAb1sM2vXcEGu+rMZJ8XIG4H7 bao@delta"
    "ecdsa-sha2-nistp521 AAAAE2VjZHNhLXNoYTItbmlzdHA1MjEAAAAIbmlzdHA1MjEAAACFBAEY1DRzmt77NPp64/DEkuPwdOd5yzchRBDmZJBvu+ip42tV3c1FdDNs/7ictpROqfm/Nkh6D/mmbQV+MI2Aa1H66AC+8/wMJl/fkfSEeX6Z6thVM7oC6a2kXuJjlZNF5wQngTP5AWydEFacuZduW+Ir2++IVFyjtIVOR/lumtdzuvi5/A== Tony@tomoko"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKAjEvrKkGk5kX7pQEILjZ3d57C8l7rpa9RH+5ynj+HQ Theta ED25519"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACQQCuvdbIeDhfVgfp5rt3/II64jtB4WCpxRrW/JarbNlwAs+U/OLJAc9YM/vFUCr3Oenn11n3qrkMXrnPawqBaqIy7JDO0c0ox75f/SZc5k0TK26dmEEywFKUUXYUDsqQn38YdCWPNy8hGHrfL+pfUCjKGbVBB2H353bHlEur15iCdcYtA/DU4yUaozs3CZWDKVR18lZPmJHtmugIDJhR6A4FOJUGld2FETXMHRPxpn2pvk+SM8xiqiCk4j1TUEixq5dAe51iVOfXg8bt8nipGlpPx4YMVY4eGFPRw+qPqws209xyJiyVt2WIa5rN924llVfPZZQr5iVm5UvPUlpWP7ogtClz+u3c9tMFDM1EUqT/Xe5iSJG1loKRKHeYR2aMR6db8lc7ke8EuyqIAcHSwefEqFLXDM7gCvSmDU+6/ynV63tQSzTgQU+FBNZX+FW+Xpr66qIoYHaAyzlfO64OyyyArux/HVEwQgupOx93LzyiKzwgQqbVQLEbD2P3RyLrxeSIxlSBZACvVl7A7daTWWeZ9+TA/k6Im/9jBTnlJTzRMlaknR2gYwJ7dCtduRF0/4fZrkVQ/gJ0xjOOBhPbM3x2P86f+ZSWd81PdVz2cD/1O+PJCFpdsgrZsiEpcXIjqJeU9aND7k0lge2OFV5iRq12kpU6iXkRUVUVKHDloJkmTx79mn3Qn2ZwVt7+pbJ1XbM+1xdEsASXXQCTeZjLkNA5fAEnKmv1S4QnfGE48FSPDqowuojCBiD/+QyusVmZmOU= Theta RSA"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDX40Dm37dDdxreqQxQ+bd9c9mNXfSqC5k3Rrf4iQHy7OdzsQD0qngHFdcq2dG0d4sYeHeyJGYHPbtwNYb0AED/zDVgfuiW6+xYSW+apg5Hc2EfktxxmVbRXqq8SoWUP9CtwJLIw0Q4vBiWBKT367VlXnWJ5ob8bwhfRlK7Sk4595Mm2Wp98sgMXdcQbFc+nQ+TYyYnkIYv63g3Lyxi8NQKWGwh2zspGl2rbDVJrtr196bSd3leZhD/v+YquqO2Saf9DZCmDMJmw4rUWZ84rkUZ0lB5eSICfQZAl5UxpnorWutLDxjuFOc/7H6iSGFRNjQ/fM8Jh2dxDm3XWxWp+E+F9q5Q9bsziL+zwxkAL2SvWULhmAALbLMqEC2qkPU9ccqRRBteGIRt/ix+J87lImz6Zp2UqSYFe2GpwP+NtMB8TyMaDUOni+L/NGw7o/EvAm7K7tH6OJW5vSC7e8P5lLc825SHlJdfn5L2aAfD+vr8rZv4L9uy1isJTMvVDRp4CLHwsw+xn4XfnfvsNtWNyVoAfi7NN3+jtCGlpNckj+5ylaQcgGuFbQUJ7jhsUGDYRSvX3wWaRBD94Pi+XS1jUTLyRmIbgcbiSeyzx2fZm1saQAb3MSF9yf0ibCJlTJ3JMLfqlDYP6Yl64bQ67T8QcYvDSUurqh+T6AN83SF6aHT3HQ== openpgp:0xBF0F46CF"
  ];
  users.users.leaf.openssh.authorizedKeys.keys = config.users.users.root.openssh.authorizedKeys.keys;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.09"; # Did you read the comment?
}
