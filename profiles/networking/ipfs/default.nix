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
}
