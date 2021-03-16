{ config, pkgs, lib, usr, ... }:

let
  cfg = config.services.mastodon;
  securityLimits = config.environment.etc.limits;
  hostAddress = "10.6.0.1";
  localAddress = "10.6.0.2";
in {
  services.postgresql.enable = true;
  services.postgresql.ensureUsers = [
    {
      name = cfg.database.user;
      ensurePermissions."DATABASE ${cfg.database.name}" = "ALL PRIVILEGES";
    }
  ];
  services.postgresql.ensureDatabases = [ cfg.database.name ];

  security.acme.acceptTerms = true;

  containers.mastodon =
    {
      autoStart = true;
      enableTun = true;
      privateNetwork = true;
      inherit hostAddress localAddress;

      config =
        { config, stdenv, ... }:

        {
          nixpkgs.pkgs = pkgs;
          nixpkgs.config.allowUnfree = true;

          environment.etc.limits = securityLimits;
          environment.systemPackages = with pkgs; [
            postgresql redis postfix config.services.mastodon.package vim wget
          ];

          services.elasticsearch.enable = true;
          services.mastodon.enable = true;
          services.mastodon.automaticMigrations = false;
          services.mastodon.extraConfig = {
            EMAIL_DOMAIN_WHITELIST = lib.concatStringsSep "|" [
              usr.secrets.domains.home
             #usr.secrets.domains.wife
            ];
            ALTERNATE_DOMAINS = lib.concatStringsSep "," [
              "mastodon.${usr.secrets.domains.srvc}"
              "microblog.${usr.secrets.domains.srvc}"
              "ublog.${usr.secrets.domains.srvc}"
            ];
            WEB_DOMAIN = "u.${usr.secrets.domains.srvc}";
          };
          services.mastodon.localDomain = usr.secrets.domains.srvc;
          services.mastodon.redis = {
            createLocally = true;
          };
          services.mastodon.database = {
            createLocally = false;
            host = hostAddress;
          };
          services.mastodon.smtp = {
            createLocally = true;
            fromAddress = "mastodon@${usr.secrets.domains.srvc}";
            user = "mastodon";
          };
          services.mastodon.configureNginx = true;
          services.mastodon.package = pkgs.mastodon;
          services.postfix.submissionOptions = {
            mynetworks = "127.0.0.0/8 10.0.0.0/8";
          };
          services.nginx = {
            enable = true;
            enableReload = true;
            virtualHosts."${cfg.localDomain}" = {
              #enableACME = lib.mkForce false;
              serverAliases = [
                "u.${usr.secrets.domains.srvc}"
                usr.secrets.domains.srvc
                "localhost"
                "127.0.0.1"
                localAddress
              ];
              listen = [
                { addr = "0.0.0.0"; port = 80; }
                { addr = "0.0.0.0"; port = 8443; ssl = true; }
              ];
            };
          };

          networking.firewall.enable = false;
          networking.nameservers = [ "62.210.16.6" "62.210.16.7" ];
          networking.extraHosts = ''${localAddress}'';

          security.acme.acceptTerms = true;
          security.acme.email = "ssl@${usr.secrets.domains.home}";
        };
      bindMounts = {
        "/var/lib/mastodon" = {
          hostPath = "/var/lib/mastodon";
          isReadOnly = false;
        };
        "/var/lib/elasticsearch" = {
          hostPath = "/var/lib/elasticsearch";
          isReadOnly = false;
        };
      };
    };
}
