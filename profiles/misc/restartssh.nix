{ config, pkgs, lib, ... }:

let
  cfg = config.services.openssh;
in {
  options.services.openssh.restartPeriod = lib.mkOption {
    type = with lib.types; nullOr str;
    default = null;
  };
  config = lib.mkIf (cfg.restartPeriod != null) {
    # Bit meta but helps ensure sshd is bound to all addresses always
    # TODO: move to profile
    systemd.services.restart-openssh.script = if cfg.startWhenNeeded
    then "${pkgs.systemd}/bin/systemctl restart sshd.socket"
    else "${pkgs.systemd}/bin/systemctl restart sshd.service";
    systemd.timers.restart-openssh = {
      timerConfig = {
        OnCalendar = cfg.restartPeriod;
        Unit = "restart-openssh.service";
      };
    };
  };
}
