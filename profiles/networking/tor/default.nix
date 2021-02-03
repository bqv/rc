{ config, pkgs, usr, ... }:

{
  services.tor = {
    enable = true;
    client.enable = true;
    client.dns.enable = true;
    client.transparentProxy.enable = true;
    client.socksListenAddress = { addr = "0.0.0.0"; port = 9050; };
    controlSocket.enable = true;
    relay.enable = true;
    relay.role = "relay";
    settings = let inherit (usr.secrets) domains; in {
      Address = domains.srvc;
      ContactInfo = "tor+${domains.srvc}@${domains.home}";
      Nickname = config.networking.hostName;
      ORPort = 143;
      ControlPort = [ 9051 ];
    };
  };
}
