{ config, lib, pkgs, ... }:

let
  cfg = config.services.hydroxide;
in {
  options.services.hydroxide = {
    enable = mkEnableOption "hydroxide";

    directory = lib.mkOption {
      type = lib.types.path;
      default = "/var/lib/hydroxide";
      description = ''
        The directory for data
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

    systemd.services.hydroxide = let
    in {
      enable = true;
      after = [ "network.target" ];
      description = "Hydroxide bridge server";
      environment.HOME = cfg.directory;
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStartPre = ''
          ${pkgs.coreutils}/bin/mkdir -p ${workdir} ${cfg.directory}
        '';
        ExecStart = let
          url = if ((cfg.github.owner == null) || (cfg.github.repo == null))
                then cfg.remote else githubRemote;
          params = cfg.extraParams // {
            branch = cfg.branch;
            foreground = "true";
            allow_other = "true";
          };
          paramsString = concatStringsSep "," (
            lib.mapAttrsToList (k: v: "${k}=${builtins.toString v}") params);
        in ''
          ${cfg.package}/bin/gitfs ${url} -o ${paramsString} ${cfg.directory}
        '';
        Restart = "always";
        RestartSec = 15;
      };
    };
  };
}
