{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.nix-index;

  package = config.programs.nix-index.package;

  renderTimer = desc: boot: active: cal: {
    description = "${desc}";
    wantedBy = [ "timers.target" ];
    timerConfig = lib.optionalAttrs (boot != "") { OnBootSec = "${boot}"; }
      // lib.optionalAttrs (active != "") { OnUnitActiveSec = "${active}"; }
      // lib.optionalAttrs (cal != "") { OnCalendar = "${cal}"; };
  };
in

{
  options.services.nix-index = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to enable nix-index and it's command-not-found helper.";
    };
  };

  config = mkIf cfg.enable {

    programs.nix-index.enable = mkDefault true;

    systemd.user.services."nix-update-index" = {
      description = "Update nix packages metadata index";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${package}/bin/nix-index";
        StandardOutput = "journal";
        StandardError = "journal";
      };
    };

    systemd.user.timers."nix-update-index" = builtins.trace
      "nix-update-index.timer: suspended cause it breaks my internet"
      (renderTimer "Update nix packages metadata index" "1h" "12h" "");

  };
}
