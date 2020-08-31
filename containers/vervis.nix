{ config, pkgs, lib, domains, ... }:

let
  hostAddress = "10.10.0.1";
  localAddress = "10.10.0.2";

  databaseUser = "vervis";
  databaseName = "vervis";
in {
  services.postgresql.enable = true;
  services.postgresql.ensureUsers = [
    {
      name = databaseUser;
      ensurePermissions."DATABASE ${databaseName}" = "ALL PRIVILEGES";
    }
  ];
  services.postgresql.ensureDatabases = [ databaseName ];

  containers.vervis =
    {
      autoStart = true;
      enableTun = true;
      privateNetwork = true;
      inherit hostAddress localAddress;

      config =
        { config, stdenv, ... }:

        let
          settingsJson = pkgs.runCommand "vervis-settings.json" {
            inherit (pkgs.vervis) src;
            buildInputs = with pkgs; [ yj ];
          } ''
            yj -y < $src/config/settings-default.yaml > $out
          '';
        in {
          options = {
            vervis.settings = lib.mkOption rec {
              type = (pkgs.formats.yaml {}).type;
              default = builtins.fromJSON example;
              example = builtins.readFile settingsJson;
            };
            vervis.dataDir = lib.mkOption {
              type = lib.types.path;
              default = "/var/lib/vervis";
            };
          };

          config = {
            nixpkgs.pkgs = pkgs;

            networking.firewall.enable = false;

            environment.systemPackages = with pkgs; [
              vervis yq yj jq git darcs
            ];

            environment.etc = let
              toYaml = lib.generators.toYAML {};
              settings = lib.foldl (json: update: lib.recursiveUpdate json update)
                (builtins.fromJSON (builtins.readFile settingsJson)) [
                  config.vervis.settings
                ];
            in {
              "vervis/settings.yml".text = toYaml settings;
              "vervis/ssh-host-key".source = "/etc/ssh/ssh_host_rsa_key";
            };

            vervis.settings = {
              registration = true;
              max-accounts = 1;
              federation = true;
            };

            systemd.services.vervis = {
              path = with pkgs; [ git darcs ];
              environment = {
                XDG_CACHE_HOME = config.vervis.dataDir;
                HOME = config.vervis.dataDir;

                PORT = "3000";
                INSTANCE_HOST = "dev.${domains.home}";
                IP_FROM_HEADER = "true";

                PGUSER = databaseUser;
               #PGPASS = "";
                PGHOST = hostAddress;
                PGPORT = toString 5432;
                PGDATABASE = "vervis";
              };
              serviceConfig = {
                ExecStartPre = pkgs.writeShellScript "vervis-init" ''
                  mkdir -p ${config.vervis.dataDir}/static
                  if [ ! -x ${config.vervis.dataDir}/config ]; then
                    ln -sf /etc/vervis ${config.vervis.dataDir}/config
                  fi
                  mkdir -p ${config.vervis.dataDir}/repos
                  if [ ! -x ${config.vervis.dataDir}/vervis ]; then
                    ln -sf ${config.vervis.dataDir} ${config.vervis.dataDir}/vervis
                  fi
                '';
                ExecStart = "${pkgs.vervis}/bin/vervis";
                WorkingDirectory = config.vervis.dataDir;
              };
              wantedBy = [ "default.target" ];
            };
          };
        };
      bindMounts = {
        "/var/lib/vervis" = {
          hostPath = "/var/lib/vervis";
          isReadOnly = false;
        };
        "/etc/ssh" = {
          hostPath = "/etc/ssh";
          isReadOnly = true;
        };
      };
    };

  system.activationScripts.var-lib-vervis = ''
    mkdir -p /var/lib/vervis
  '';
}
