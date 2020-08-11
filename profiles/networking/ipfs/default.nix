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
    serviceConfig.ExecStartPre = "${pkgs.coreutils}/bin/rm -rf /var/lib/ipfs/*";
  };

  systemd.services.ipfs = builtins.trace "ipfs config permissions still broken" {
    serviceConfig.ExecStartPost = "${pkgs.coreutils}/bin/chmod g+r /var/lib/ipfs/config";
  };
}
