{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.swc-launch;

  libswc = (import ../../pkgs/libswc/default.nix);
in

{
  imports = [
    ./swc-servers/default.nix
  ];

  ###### interface

  options = {
    # TODO: Since there is no login screen (yet), add option to specify login user.
    services.swc-launch = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable a Wayland compositor launched with swc-launch.
        '';
      };

      layout = mkOption {
        type = types.str;
        default = "us";
        description = ''
          Keyboard layout.
        '';
      };

      xkbOptions = mkOption {
        type = types.str;
        default = "";
        example = "grp:caps_toggle, grp_led:scroll";
        description = ''
          xkb keyboard options.
        '';
      };

      tty = mkOption {
        type = types.int;
        default = 9;
        description = "Virtual console for swc-launch to use.";
      };

      user = mkOption {
        type = types.str;
        default = null;
        description = "User to run swc-launch as.";
      };
    };
  };


  ###### implementation

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ velox ] ++ (with velox; [ swc wld ]);

    environment.etc = {
      "velox.conf".source = "${pkgs.velox}/etc/velox.conf";
    };

    systemd.services.swc-launch = {
      enable = true;
      description = "Launcher for libswc-based Wayland compositors";
      after = [ "systemd-udev-settle.service" "local-fs.target" ];

      restartIfChanged = false;

      serviceConfig = {
        User = "${cfg.user}";
      };

      environment = {
        XKB_DEFAULT_LAYOUT = "${cfg.layout}";
        XKB_DEFAULT_OPTIONS = "${cfg.xkbOptions}";

        # FIXME: This doesn't work when the user hasn't explicitly set her uid.
        XDG_RUNTIME_DIR = "/run/user/${toString config.users.extraUsers.${cfg.user}.uid}";
      };

      script = with cfg; ''
        ${config.security.wrapperDir}/swc-launch -t /dev/tty${toString tty} \
        -- ${server.${server.active_server}.command}
      '';
    };

    # needs setuid in order to manage tty's
    security.wrappers.swc-launch = {
      source = "${pkgs.velox.swc}/bin/swc-launch";
      capabilities = "cap_sys_tty_config+ep";
    };
  };
}
