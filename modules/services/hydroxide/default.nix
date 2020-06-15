{ config, lib, pkgs, ... }:

let
  cfg = config.services.hydroxide;
in {
  options.services.hydroxide = {
    enable = mkEnableOption "hydroxide";

    directory = lib.mkOption {
      type = lib.types.path;
      default = "/var/lib/";
      description = ''
        The directory for the ./hydroxide/ data directory
      '';
    };

    userauths = lib.mkOption {
      type = with lib.types; attrsOf str;
      default = {};
      description = ''
        Authentication data of registered users
      '';
    };

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.hydroxide;
      defaultText = "pkgs.hydroxide";
      description = ''
        Which package to use.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    systemd.services.hydroxide = {
      enable = true;
      after = [ "network.target" ];
      description = "Hydroxide bridge server";
      environment.XDG_CONFIG_HOME = cfg.directory;
      wantedBy = [ "multi-user.target" ];
      serviceConfig = rec {
        DynamicUser = true;
        StateDirectory = "${cfg.directory}/hydroxide";
        ExecStartPre = let
          authFile = builtins.toJSON cfg.userauths;
        in ''
          ln -sf ${pkgs.writeText "auth.json" authFile} ${StateDirectory}/auth.json
        '';
        ExecStart = ''
          ${cfg.package}/bin/hydroxide ${""} serve
        '';
        Restart = "always";
        RestartSec = 15;
      };
    };
  };
}
