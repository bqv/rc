{ config, pkgs, ... }:

{
  containers.secure = {
    autoStart = true;
    enableTun = true;
    privateNetwork = true;
    hostAddress = "10.2.0.1";
    localAddress = "10.2.0.2";
    config =
      { config, ... }:
      {
      };
  };
}
