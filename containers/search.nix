{ config, pkgs, ... }:

{
  containers.search =
    {
      autoStart = true;
      enableTun = true;
      privateNetwork = true;
      hostAddress = "10.5.0.1";
      localAddress = "10.5.0.2";
      config =
        { config, stdenv, ... }:
 
        {
          systemd.services.yacy =
            with pkgs; {
              description = "Yacy P2P Search Engine";
              after = [ "network.target" ];
              path = [
                which
                getopt
                openjdk
              ];
              environment = {
                YACY_DATA_PATH = "${pkgs.yacy}/yacy";
                YACY_PARENT_DATA_PATH = "${pkgs.yacy}/yacy";
              };
              serviceConfig = {
                WorkingDirectory = "${pkgs.yacy}/yacy";
                TimeoutStopSec = "50";
                RestartSec = "3";
              };
              script = "set -x; . ${pkgs.yacy}/yacy/env.sh; ${pkgs.openjdk}/bin/java $JAVA_ARGS -classpath $CLASSPATH net.yacy.yacy";
              preStop = "set -x; . ${pkgs.yacy}/yacy/env.sh; ${pkgs.openjdk}/bin/java $JAVA_ARGS -cp $CLASSPATH net.yacy.yacy -shutdown";
              wantedBy = [ "multi-user.target" ];
            };
 
          networking.firewall.enable = false;
        };
      bindMounts = {
        "/var/lib/yacy" = {
          hostPath = "/var/lib/yacy";
          isReadOnly = false;
        };
      };
    };
}
