{ config, pkgs, ... }:

{
  services.ipfs = {
    enable = true;
    enableGC = true;
    #emptyRepo = true;
    extraConfig = {
      DataStore = {
        StorageMax = "100GB";
      };
      Discovery = {
        MDNS.Enabled = true;
        Swarm.AddrFilters = null;
      };
    };
  };
}
