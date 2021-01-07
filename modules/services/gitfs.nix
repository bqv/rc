{ config, lib, pkgs, ... }:

let
  cfg = config.services.gitfs;
in {
  options.services.gitfs = with lib; {
    enable = mkEnableOption "gitfs";

    workdir = mkOption {
      type = types.str;
      default = "/var/lib/gitfs";
      description = ''
        The internal gitfs working directory.
      '';
    };

    mounts = mkOption {
      type = types.attrsOf (types.submodule ({ name, config, ... }: {
        options = {
          directory = mkOption {
            type = types.path;
            default = name;
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
              example = "nixrc";
              description = ''
                Name of the repository hosted on GitHub.
              '';
            };

            remote = mkOption {
              type = types.str;
              internal = true;
              default = with config.github; "http://github.com/${owner}/${repo}";
            };
          };

          remote = mkOption {
            type = with types; nullOr str;
            default = null;
            example = "http://github.com/bqv/nixrc";
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

          invocation = mkOption {
            type = types.submodule {
              options = {
                url = mkOption { type = types.str; };
                params = mkOption { type = types.str; };
              };
            };
            internal = true;
            default = {
              url = if ((config.github.owner == null) || (config.github.repo == null))
                    then config.remote else config.github.remote;
              params = let
                paramAttrs = config.extraParams // {
                  inherit (config) branch;
                  foreground = "true";
                  allow_other = "true";
                };
                toKVList = lib.mapAttrsToList (k: v: "${k}=${builtins.toString v}");
              in concatStringsSep "," (toKVList paramAttrs);
            };
          };
        };
      }));
      default = {
      };
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

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = builtins.all (mnt: !(builtins.all (v: v == null)
        [ mnt.github.owner mnt.github.repo mnt.remote ])) (builtins.attrValues cfg.mounts);
        message = ''
          You must set either remote or github in services.gitfs.mounts.
        '';
      }
      {
        assertion = builtins.all (mnt: ((mnt.github.owner == null) == (mnt.github.repo == null))) (builtins.attrValues cfg.mounts);
        message = ''
          You must set both owner and repo in services.gitfs.github.mounts.
        '';
      }
    ];

    environment.systemPackages = [ cfg.package ];

    programs.fuse.userAllowOther = true;

    systemd.services = lib.mapAttrs' (name: mnt: {
      name = lib.strings.sanitizeDerivationName "gitfs-${name}";
      value = {
        enable = true;
        path = [ cfg.package pkgs.coreutils ];
        after = [ "network.target" ];
        description = "${mnt.directory} gitfs mount of ${mnt.invocation.url}";
        environment.HOME = cfg.workdir;
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          ExecStartPre = ''
            ${pkgs.coreutils}/bin/mkdir -p ${cfg.workdir} ${mnt.directory}
          '';
          ExecStart = ''
            ${cfg.package}/bin/gitfs ${mnt.invocation.url} -o ${mnt.invocation.params} ${mnt.directory}
          '';
          Restart = "always";
          RestartSec = 15;
        };
      };
    }) cfg.mounts;
  };
}
