{ config, pkgs, lib, ... }:

let
  cfg = config.services.openssh;
in {
  options.services.openssh.restartPeriod = lib.mkOption {
    type = with lib.types; nullOr str;
    default = null;
  };
  config = {
    # Bit meta but helps ensure sshd is bound to all addresses always
    systemd.services.restart-openssh.serviceConfig.Type = "oneshot";
    systemd.services.restart-openssh.serviceConfig.ExecStart = if cfg.startWhenNeeded
    then "systemctl restart sshd.socket"
    else "systemctl restart sshd.service";
    systemd.timers.restart-openssh = lib.mkIf (cfg.restartPeriod != null) {
      timerConfig = {
        OnCalendar = cfg.restartPeriod;
        Unit = "restart-openssh.service";
      };
      wantedBy = [ "timers.target" ];
    };
  };
}
