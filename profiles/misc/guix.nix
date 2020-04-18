{ config, pkgs, lib, ...}:

let
  cfg = config.services.guix;

  guixBuildUser = id: {
    name = "${cfg.userPrefix}${toString id}";
    createHome = false;
    description = "Guix build user ${toString id}";
    extraGroups = [ cfg.group ];
    isSystemUser = true;
  };

  guixBuildUsers = numberOfUsers:
    builtins.listToAttrs
      (map (user: {
        name = user.name;
        value = user;
      }) (builtins.genList guixBuildUser numberOfUsers));

  guixEnv = {
    GUIX_STATE_DIRECTORY = "/gnu/var";
    GUIX_LOG_DIRECTORY = "/gnu/var/log";
    GUIX_DATABASE_DIRECTORY = "/gnu/var/db";
    NIX_STORE_DIR = "/gnu/store";
  };

  guixWrapped = pkgs.writeShellScriptBin "guix" ''
    ${lib.concatStringsSep "\n"
      (lib.mapAttrsToList (k: v: "export ${k}=${v}") guixEnv)}
    exec ${cfg.package}/bin/guix $@
  '';
in {
  options.services.guix = with lib; {
    enable = mkEnableOption "the guix daemon and init /gnu/store";

    group = mkOption {
      type = types.str;
      default = "guixbuild";
      example = "guixbuild";
      description = ''
        The group of the guix build users.
      '';
    };

    userPrefix = mkOption {
      type = types.str;
      default = "guixbuilder";
      example = "guixbuilder";
      description = ''
        The common prefix of the guix build users.
      '';
    };

    extraArgs = mkOption {
      type = with types; listOf str;
      default = [];
      example = [ "--max-jobs=4" "--debug" ];
      description = ''
        Extra flags to pass to the guix daemon.
      '';
    };

    package = mkOption {
      type = types.package;
      default = pkgs.guix;
      defaultText = "pkgs.guix";
      description = ''
        Guix package to use.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      guixWrapped
    ];

    users.users = guixBuildUsers 10;
    users.groups = { "${cfg.group}" = {}; };

    systemd.services.guix-daemon = {
      script = ''
        export GUIX_CONFIGURATION_DIRECTORY=$RUNTIME_DIRECTORY
        ${guixWrapped}/bin/guix archive --authorize < \
          ${cfg.package}/share/guix/berlin.guixsd.org.pub
        ${guixWrapped}/bin/guix archive --authorize < \
          ${cfg.package}/share/guix/ci.guix.gnu.org.pub
        ${guixWrapped}/bin/guix archive --authorize < \
          ${cfg.package}/share/guix/ci.guix.info.pub

        ${lib.concatStringsSep "\n"
          (lib.mapAttrsToList (k: v: "export ${k}=${v}") guixEnv)}
        DAEMON="/var/guix/profiles/per-user/root/current-guix/bin/guix-daemon"
        if [ ! -x "$DAEMON" ]; then
          DAEMON="${cfg.package}/bin/guix-daemon"
          export GUIX_LOCPATH="${pkgs.glibcLocales}/lib/locale"
        fi

        exec $DAEMON --build-users-group=${cfg.group} ${lib.concatStringsSep " " cfg.extraArgs}
      '';
      environment.GUIX_LOCPATH = "/var/guix/profiles/per-user/root/guix-profix/lib/locale";
      serviceConfig = {
        ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p /gnu/store";
        RuntimeDirectory = "guix";
        StandardOutput = "syslog";
        StandardError = "syslog";
        TasksMax = 8192;
      };
      wantedBy = [ "multi-user.target" ];
    };

    environment.shellInit = ''
      export GUIX_PROFILE="$HOME/.config/guix/current"
      [ -f "$GUIX_PROFILE/etc/profile" ] && source $GUIX_PROFILE/etc/profile
      export GUIX_LOCPATH="${pkgs.glibcLocales}/lib/locale"
      export INFOPATH="$GUIX_PROFILE/share/info:$INFOPATH"
    '';
  };
}
