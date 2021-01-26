{ config, pkgs, lib, ... }:

{
  fileSystems."/net/share" = {
    device = "//<IP_OR_HOST>/path/to/share";
    fsType = "cifs";
    options = let
      # this line prevents hanging on network split
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";

    in ["${automount_opts},credentials=${pkgs.writeText "smb-secrets" ''
      username=<USERNAME>
      domain=<DOMAIN>
      password=<PASSWORD>
    ''}"];
  };

  environment.systemPackages = [ pkgs.lxqt.lxqt-policykit ];

  networking.firewall.allowPing = true;
  networking.firewall.allowedTCPPorts = [ 445 139 ];
  networking.firewall.allowedUDPPorts = [ 137 138 ];

  services.samba = {
    enable = true;
    package = if config.services.printing.enable then pkgs.sambaFull else pkgs.samba;
    securityType = "user";
    extraConfig = ''
      workgroup = WORKGROUP
      server string = smbnix
      netbios name = smbnix
      security = user
      #use sendfile = yes
      #max protocol = smb2
      hosts allow = 192.168.0  localhost
      hosts deny = 0.0.0.0/0
      guest account = nobody
      map to guest = bad user
    '' ++ (lib.optionalString config.services.printing.enable '';
      load printers = yes
      printing = cups
      printcap name = cups
    '');
    shares = {
      public = {
        path = "/net/Shares/Public";
        browseable = "yes";
        writeable = "yes";
        "read only" = "no";
        "guest ok" = "yes";
        "create mask" = 0644;
        "directory mask" =  0755;
        "force user" = "username";
        "force group" = "groupname";
      };
      private = {
        path = "/net/Shares/Private";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "no";
        "create mask" = 0644;
        "directory mask" = 0755;
        "force user" = "username";
        "force group" = "groupname";
      };
      printers = {
        comment = "Printers";
        path = "/var/spool/samba";
        public = "yes";
        browseable = "yes";
        "guest ok" = "yes";
        writable = "no";
        printable = "yes";
        "create mode" = 0700;
      };
    };
  };

  services.gvfs = {
    enable = true;
    package = lib.mkForce pkgs.gnome3.gvfs;
  };

  systemd.tmpfiles.rules = lib.optionals config.services.printing.enable [
    "d /var/spool/samba 1777 root root -"
  ];
}
