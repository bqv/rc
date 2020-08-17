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
        config._module.freeformType = jsonFormat.type;

        options.id = lib.mkOption { type = lib.types.str; };
        options.private_key = lib.mkOption { type = lib.types.str; };
      };
      default = (import ../../../secrets/ipfs.cluster.nix).${config.networking.hostName};
      description = "The IPFS Cluster node identity credentials";
    };

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.ipfs-cluster;
      description = "The package to use for IPFS Cluster";
    };

    settings = lib.mkOption {
      default = builtins.fromJSON (builtins.readFile ./default.json);
      type = jsonFormat.type;
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
        ln -s ${pkgs.writeText "services.json" (builtins.toJSON cfg.settings)} \
          ${cfg.dataDir}/services.json
        ln -s ${pkgs.writeText "identity.json" (builtins.toJSON cfg.identity)} \
          ${cfg.dataDir}/identity.json
        #touch peerstore
      '';
      wantedBy = config.systemd.services.ipfs-cluster.wantedBy;
    };

    systemd.services.ipfs-cluster = {
      enable = true;
      after = [ "network-online.target" "ipfs-cluster-init" ];
      wants = [ "ipfs-cluster-init" ];
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
      wantedBy = [ "multi-user.target" ];
    };
  };
}
