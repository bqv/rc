{ config, pkgs, lib, domains, fetchPullRequest, ... }:

let
  cfg = config.services.mastodon;
  securityLimits = config.environment.etc.limits;
  hostAddress = "10.6.0.1";
  localAddress = "10.6.0.2";

  twitterCfg = with import ../secrets/mastodon.twitter.nix; {
    inherit key crt;
    keyFile = pkgs.writeText "selfsigned.key" key;
    crtFile = pkgs.writeText "selfsigned.crt" crt;
  };
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
          imports = [
            ../profiles/services/mastodon
          ];

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
              domains.home
             #domains.wife
            ];
            ALTERNATE_DOMAINS = lib.concatStringsSep "," [
              "mastodon.${domains.srvc}"
              "microblog.${domains.srvc}"
              "ublog.${domains.srvc}"
            ];
            WEB_DOMAIN = "u.${domains.srvc}";
          };
          services.mastodon.localDomain = domains.srvc;
          services.mastodon.redis = {
            createLocally = true;
          };
          services.mastodon.database = {
            createLocally = false;
            host = hostAddress;
          };
          services.mastodon.smtp = {
            createLocally = true;
            fromAddress = "mastodon@${domains.srvc}";
          };
          services.mastodon.configureNginx = true;
          services.mastodon.package = with fetchPullRequest {
            id = 78810;
            sha256 = "0ksq5dwmlmz1vp40g1hq0bjxyczggi1k0rrkmcyaj8kk85qazx1f";
          }; mastodon;
          services.postfix.submissionOptions = {
            mynetworks = "127.0.0.0/8 10.0.0.0/8";
          };
          services.nginx = {
            enable = true;
            enableReload = true;
            virtualHosts."${cfg.localDomain}" = {
              #enableACME = lib.mkForce false;
              serverAliases = [
                "u.${domains.srvc}"
                domains.srvc
                "localhost"
                "127.0.0.1"
                localAddress
              ];
              listen = [
                { addr = "0.0.0.0"; port = 80; }
                { addr = "0.0.0.0"; port = 443; ssl = true; }
              ];
            };
          };

          networking.firewall.enable = false;
          networking.nameservers = [ "62.210.16.6" "62.210.16.7" ];
          networking.extraHosts = ''${localAddress} twitter.com'';

          security.acme.acceptTerms = true;
          security.acme.email = "ssl@${domains.home}";
          security.pki.certificates = [ twitterCfg.crt ];

          boot.enableContainers = true;
          boot.kernel.sysctl = {
            "net.ipv4.ip_forward" = "1";
          };
          containers.twitterpub =
            {
              autoStart = true;

              config =
                { config, stdenv, ... }:

                {
                  nixpkgs.pkgs = pkgs;
                  nixpkgs.config.allowUnfree = true;

                  environment.systemPackages = with pkgs; [
                    twitterpub vim wget
                  ];

                  networking.firewall.enable = false;

                  systemd.services.twitterpub = {
                    serviceConfig = let
                      configToml = pkgs.writeText "twitterpub.toml" ''
                        Domain = "twitter.com"
                        Listen = ":80"
                        TLS = true
                        CertFile = "${twitterCfg.crtFile}"
                        KeyFile = "${twitterCfg.keyFile}"
                      '';
                    in rec {
                      WorkingDirectory = "/var/lib/twitterpub";
                      ExecStartPre = pkgs.writeShellScript "setup-twitterpub" ''
                        mkdir -p ${WorkingDirectory}
                        ln -sf ${configToml} ${WorkingDirectory}/twitterpub.toml
                        ln -sf ${pkgs.twitterpub.src}/main.html ${WorkingDirectory}/main.html
                      '';
                      ExecStart = "${pkgs.twitterpub}/bin/twitterpub";
                      Restart = "always";
                    };
                    wantedBy = [ "default.target" ];
                  };
                };
              bindMounts = {
                "/var/lib/twitterpub" = {
                  hostPath = "/var/lib/mastodon/twitterpub";
                  isReadOnly = false;
                };
              };
            };
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
