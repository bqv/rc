{ config, lib, pkgs, ... }:

let
  cfg = config.services.ipfs-cluster;

  jsonFormat = pkgs.formats.json {};
in {
  options.services.ipfs-cluster = {
    enable = lib.mkEnableOption "ipfs cluster peer";

    dataDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/ipfs-cluster";
      description = "The data dir for IPFS Cluster";
    };

    identity = lib.mkOption {
      type = lib.types.submodule {
        config._module.freeformType = (pkgs.formats.json {}).type;

        options.id = lib.mkOption { type = lib.types.str; description = "Id."; };
        options.private_key = lib.mkOption { type = lib.types.str; description = "Private key."; };
      };
      description = "The IPFS Cluster node identity credentials";
    };

    identityFile = lib.mkOption {
      type = lib.types.path;
      default = pkgs.writeText "identity.json" (builtins.toJSON cfg.identity);
      description = "Identity json file path.";
    };

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.ipfs-cluster;
      description = "The package to use for IPFS Cluster";
    };

    settings = lib.mkOption {
      example = builtins.readFile ./default.json;
      type = jsonFormat.type;
      description = "Settings json content.";
    };

    settingsFile = lib.mkOption {
      type = lib.types.path;
      default = let
        settings = lib.foldl (json: update: lib.recursiveUpdate json update)
          (builtins.fromJSON (builtins.readFile ./default.json)) [
            (lib.setAttrByPath ["cluster" "peername"] config.networking.hostName)
            cfg.settings ];
        in pkgs.writeText "service.json" (builtins.toJSON settings);
      description = "Settings json file path.";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.ipfs-cluster ];

    systemd.services.ipfs-cluster-init = {
      enable = true;
      description = "Setup ${config.systemd.services.ipfs-cluster.description}";
      script = ''
        mkdir -p ${cfg.dataDir}
        chown ${config.services.ipfs.user}:${config.services.ipfs.group} ${cfg.dataDir}
        rm -f ${cfg.dataDir}/service.json ${cfg.dataDir}/identity.json
        ln -sf ${cfg.settingsFile} ${cfg.dataDir}/service.json
        ln -sf ${cfg.identityFile} ${cfg.dataDir}/identity.json
      '';
      restartTriggers = [ cfg.settingsFile cfg.identityFile ];
      wantedBy = config.systemd.services.ipfs-cluster.wantedBy;
    };

    systemd.services.ipfs-cluster = {
      enable = true;
      after = [ "network-online.target" "ipfs-cluster-init.service" ];
      wants = [ "ipfs-cluster-init.service" ];
      description = "IPFS cluster peer";
      environment.IPFS_CLUSTER_PATH = config.services.ipfs-cluster.dataDir;
      serviceConfig = {
        User = config.services.ipfs.user;
        Group = config.services.ipfs.group;
        ExecStart = ''
          ${cfg.package}/bin/ipfs-cluster-service daemon
        '';
        Restart = "always";
        RestartSec = 15;
      };
      restartTriggers = [ cfg.settingsFile ];
      wantedBy = [ "multi-user.target" ];
    };
  };
}
