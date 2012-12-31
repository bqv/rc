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

    # TODO: per-service options
    host = lib.mkOption {
      type = with lib.types; nullOr str;
      default = null;
      description = ''
        Host to bind to
      '';
    };
    port = lib.mkOption {
      type = with lib.types; nullOr int;
      default = null;
      description = ''
        Port to bind to
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
        ExecStart = let
          hostOpts = if isNull cfg.host then "" else let
            inherit (cfg) host;
          in "-imap-host ${host} -smtp-host ${host} -carddav-host ${host}";
          portOpts = if isNull cfg.port then "" else let
            inherit (cfg) port;
          in "-imap-port ${port} -smtp-port ${port} -carddav-port ${port}";
          args = lib.concatStringsSep " " [ hostOpts portOpts ];
        in ''
          ${cfg.package}/bin/hydroxide ${args} serve
        '';
        Restart = "always";
        RestartSec = 15;
      };
    };
  };
}
