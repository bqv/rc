{ config, pkgs, ... }:

with config; {
  services.tor.client.dns.enable = true;
  services.tor.client.dns.listenAddress = "0.0.0.0:9053";
  services.tor.client.enable = true;
  services.tor.client.socksListenAddress = "0.0.0.0:9050";
  services.tor.client.socksListenAddressFaster = "0.0.0.0:9090";
  services.tor.client.transparentProxy.enable = true;
  services.tor.client.transparentProxy.listenAddress = "0.0.0.0:9040";
  services.tor.controlPort = "9051";
  services.tor.controlSocket.enable = true;
  services.tor.enable = true;
  services.tor.relay.address = "xa0.uk";
  services.tor.relay.contactInfo = "xa0@fron.io";
  services.tor.relay.enable = true;
  services.tor.relay.nickname = "zeta";
  services.tor.relay.port = 143;
  services.tor.relay.role = "relay";
}
