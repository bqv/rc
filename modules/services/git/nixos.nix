{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.services.nixos-git;
  workdir = "/var/lib/gitfs";
  githubRemote = with cfg.github; "http://github.com/${owner}/${repo}";
in {
  options.services.nixos-git = {
    enable = mkEnableOption "NixOS.git";

    directory = mkOption {
      type = types.path;
      default = "/etc/nixos.git";
      description = ''
        The directory where nixos.git will be mounted.
      '';
    };

    github = {
      owner = mkOption {
        type = with types; nullOr str;
        default = null;
        example = "bqv";
        description = ''
          Owner of the repository hosted on GitHub.
        '';
      };

      repo = mkOption {
        type = with types; nullOr str;
        default = null;
        example = "nixos";
        description = ''
          Name of the repository hosted on GitHub.
        '';
      };
    };

    remote = mkOption {
      type = with types; nullOr str;
      default = null;
      example = "http://github.com/bqv/nixos";
      description = ''
        Url of the repository.
      '';
    };

    branch = mkOption {
      type = types.str;
      default = "master";
      example = "live";
      description = ''
        Name of the branch of the repository to be mounted.
      '';
    };

    extraParams = mkOption {
      type = types.attrs;
      default = {};
      example = { idle_fetch_timeout = "10"; };
      description = ''
        Extra params to pass to the mounter.
      '';
    };

    package = mkOption {
      type = types.package;
      default = pkgs.gitfs;
      defaultText = "pkgs.gitfs";
      description = ''
        Which gitfs package to use.
      '';
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = !(builtins.all (v: v == null)
          [ cfg.github.owner cfg.github.repo cfg.remote ]);
        message = ''
          You must set either remote or github in services.nixos-git.
        '';
      }
      {
        assertion = ((cfg.github.owner == null) == (cfg.github.repo == null));
        message = ''
          You must set both owner and repo in services.nixos-git.github.
        '';
      }
    ];

    programs.fuse.userAllowOther = true;

    systemd.services.nixos-git = let
    in {
      enable = true;
      path = [ cfg.package pkgs.coreutils ];
      after = [ "network.target" ];
      description = "NixOS.git Mount";
      environment.HOME = workdir;
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
