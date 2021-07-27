{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [ step-cli ];

  containers.authority = {
    autoStart = true;
    enableTun = true;
    privateNetwork = true;
    hostAddress = "10.4.0.1";
    localAddress = "10.4.0.2";
    config =
      { config, ... }:
      {
        environment.systemPackages = with pkgs; [ step-cli step-ca ];

        systemd.services.step-ca = {
          description = "Step CA Daemon";
          script = with pkgs; ''${systemd}/bin/systemd-ask-password --timeout=3600 --no-tty 'CA Password:' | ${step-ca}/bin/step-ca config/ca.json --password-file /dev/stdin'';
          serviceConfig = {
            WorkingDirectory = "/var/lib/step/";
            Restart = "always";
          };
          wantedBy = [ "multi-user.target" ];
        };

        networking.firewall.enable = false;
      };
    bindMounts = {
      "/var/lib/step" = {
        hostPath = "/var/lib/step";
        isReadOnly = false;
      };
      "/run/systemd/ask-password" = {
        hostPath = "/run/systemd/ask-password";
        isReadOnly = false;
      };
      "/run/systemd/ask-password-block" = {
        hostPath = "/run/systemd/ask-password-block";
        isReadOnly = false;
      };
    };
  };
}
