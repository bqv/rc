{ config, pkgs, lib, ... }:

{
  imports = [
    ./services/wayland/swc-launch.nix
  ];

  config = {
    environment.systemPackages = with pkgs; with velox; [
      dmenu (pkgs.lowPrio dmenu-velox)
      st-velox
      wl-clipboard clipman
      kanshi
    ];

    environment.etc = {
      "velox.conf".source = "${pkgs.velox}/etc/velox.conf";
      "greetd/config.toml".text = let
        swcd = with config.services.swc-launch; pkgs.writeShellScriptBin "wayland-session" ''
          export XKB_DEFAULT_LAYOUT = "${layout}";
          export XKB_DEFAULT_OPTIONS = "${xkbOptions}";

          # FIXME: This doesn't work when the user hasn't explicitly set her uid.
          export XDG_RUNTIME_DIR = "/run/user/${toString config.users.extraUsers.${user}.uid}";
          export XDG_SESSION_TYPE=wayland

          export MOZ_ENABLE_WAYLAND=1
          export CLUTTER_BACKEND=wayland
          export QT_QPA_PLATFORM=wayland-egl
          export ECORE_EVAS_ENGINE=wayland-egl
          export ELM_ENGINE=wayland_egl
          export SDL_VIDEODRIVER=wayland
          export _JAVA_AWT_WM_NONREPARENTING=1
          export NO_AT_BRIDGE=1

          exec ${config.security.wrapperDir}/swc-launch -t /dev/tty${toString tty} \
            -- ${server.${server.active_server}.command}
        '';
      in ''
        [terminal]
        vt = 1

        [default_session]
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd ${swcd}"
        user = "greeter"

        [initial_session]
        command = "${swcd}"
        user = "bao"
      '';
    };

    # Enable swc+velox (Wayland compositor) as alternative to X11
    services.swc-launch = {
      enable = true;
      user = "bao";
      layout = "gb";
      xkbOptions = "caps:ctrl_modifier";
      server.velox.enable = true;
      tty = 9;
    };
    systemd.services.swc-launch = {
      enable = lib.mkForce false;
      wantedBy = lib.mkForce [];
    };
    systemd.services.greetd = {
      enable = true;
      serviceConfig.ExecStart = "${pkgs.greetd}/bin/greetd";
      wantedBy = [ "multi-user.target" ];
    };
    systemd.services.display-manager = {
      enable = lib.mkForce false;
      unitConfig = {
        Before = "swc-launch.service";
        Wants = "swc-launch.service";
      };
      serviceConfig = {
        ExecStartPost = "systemctl stop display-manager.service";
      };
    };

    users.users.greeter.isSystemUser = true;
  };
}
