{ config, pkgs, ... }:

{
  services.ipfs = {
    enable = true;
    enableGC = true;
    emptyRepo = true;
    autoMount = true;
    extraConfig = {
      DataStore = {
        StorageMax = "100GB";
      };
      Discovery = {
        MDNS.Enabled = true;
        #Swarm.AddrFilters = null;
      };
    };
    gatewayAddress = "/ip4/127.0.0.1/tcp/4501";
  };

  systemd.services.ipfs-init = builtins.trace "ipfs still running ephemerally" {
    path = [ pkgs.coreutils ];
    serviceConfig.ExecStartPre = "rm -rf /var/lib/ipfs";
    serviceConfig.ExecStartPost = "chmod g+r /var/lib/ipfs/config";
  };
}
