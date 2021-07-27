{ config, pkgs, lib, ... }:

{
  options = {
    services.flake-ci.enable = lib.mkEnableOption "Flake CI";
  };
  config = lib.mkIf config.services.flake-ci.enable {
    systemd.services.flake-ci = {
      enable = true;
      description = "Flake CI";
      path = [ pkgs.nixUnstable ];
      serviceConfig.Type = "oneshot";
      serviceConfig.User = config.users.users.bao.name;
      serviceConfig.WorkingDirectory = "/srv/git/github.com/bqv/nixrc";
      serviceConfig.ExecStart = "nix develop -c forecast master small";
    };
    systemd.timers.flake-ci = {
      enable = true;
      description = "Flake CI timer";
      timerConfig = {
        OnCalendar = "hourly";
        Unit = "flake-ci.service";
      };
      wantedBy = [ "timers.target" ];
    };
  };
}
