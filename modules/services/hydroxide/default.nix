{ config, lib, pkgs, ... }:

let
  cfg = config.services.hydroxide;
in {
  options.services.hydroxide = {
    enable = lib.mkEnableOption "hydroxide";

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

    systemd.services.hydroxide = rec {
      enable = true;
      after = [ "network.target" ];
      description = "Hydroxide bridge server";
      environment.XDG_CONFIG_HOME = "/var/lib/${serviceConfig.StateDirectory}";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        DynamicUser = true;
        StateDirectory = "hydroxide";
        ExecStartPre = let
          authFile = builtins.toJSON cfg.userauths;
        in ''
          ${pkgs.coreutils}/bin/ln -sf ${pkgs.writeText "auth.json" authFile} ${environment.XDG_CONFIG_HOME}/auth.json
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
