{ config, pkgs, lib, domains, ... }:

let
  hostAddress = "10.10.0.1";
  localAddress = "10.10.0.2";

  klaus = pkgs.klaus.overrideAttrs (v: {
    propagatedBuildInputs = v.propagatedBuildInputs ++ [
      pkgs.python3.pkgs.python-ctags3
    ];
  });
in {
  environment.systemPackages = [
    klaus
  ];

  containers.klaus =
    {
      autoStart = true;
      enableTun = true;
      privateNetwork = true;
      inherit hostAddress localAddress;

      config =
        { config, stdenv, ... }:

        {
          nixpkgs.pkgs = pkgs;

          networking.firewall.enable = false;

          environment.systemPackages = (with pkgs; [
            yq yj jq git darcs pijul subversion mercurial
          ]) ++ [ klaus ];

          systemd.services.klaus = {
            path = [ pkgs.git pkgs.ctags ];
            environment = {
              PORT = "3000";
              HOST = "0.0.0.0";
              DOMAIN = "dev.${domains.home}";
              CTAGS = "tags-and-branches";
              PATHS = lib.concatMapStringsSep " " (n: "/srv/${n}") [
                "nixrc"
              ];
            };
            serviceConfig = {
              ExecStart = "${klaus}/bin/klaus --host $HOST --port $PORT --site-name $DOMAIN --ctags $CTAGS --smarthttp $PATHS";
            };
            wantedBy = [ "default.target" ];
          };
        };
      bindMounts = {
        "/srv" = {
          hostPath = "/srv/git";
          isReadOnly = false;
        };
        "/etc/ssh" = {
          hostPath = "/etc/ssh";
          isReadOnly = true;
        };
      };
    };

  systemd.tmpfiles.rules = [
    "d /srv/git 1777 root root -"
  ];
}
