{ config, lib, pkgs, ... }:

let
  cfg = config.services.ipfs-cluster;

  identity = pkgs.writeText "identity.json" (
    (import ../../../secrets/ipfs.cluster.nix).${config.networking.hostName}
  );

  peerstore = pkgs.writeText "peerstore" "";

 #service = pkgs.writeText "services.json" ''
 #'';
in {
  options.services.ipfs-cluster = {
    enable = lib.mkEnableOption "ipfs cluster peer";

    dataDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/ipfs-cluster";
      description = "The data dir for IPFS Cluster";
    };

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.ipfs-cluster;
      description = "The package to use for IPFS Cluster";
    };

    settings = let
      jsonFormat = pkgs.formats.json {};
    in lib.mkOption {
      default = builtins.fromJSON (builtins.readFile ./service.json);
      type = lib.types.submodule {
        freeformType = jsonFormat.type;
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.ipfs-cluster ];

    systemd.services.ipfs-cluster = {
      enable = true;
      after = [ "network-online.target" ];
      description = "IPFS cluster peer";
      serviceConfig = {
        User = config.services.ipfs.user;
        Group = config.services.ipfs.group;
        Environment.IPFS_CLUSTER_PATH = config.services.ipfs-cluster.dataDir;
        ExecStart = ''
          ${cfg.package}/bin/ipfs-cluster-service daemon
        '';
        Restart = "always";
        RestartSec = 15;
      };
     #wantedBy = [ "multi-user.target" ];
    };
  };
}
