{ config, lib, pkgs, ... }:

let
  channels = {
    home-manager = {
      name = "guix-home-manager";
      url = "https://framagit.org/tyreunom/guix-home-manager.git";
      introduction = "b5f32950a8fa9c05efb27a0014f9b336bb318d69";
      fingerprint = "1EFB 0909 1F17 D28C CBF9  B13A 53D4 57B2 D636 EE82";
    };
    nonguix = {
      name = "nonguix";
      url = "https://gitlab.com/nonguix/nonguix";
      introduction = "897c1a470da759236cc11798f4e0a5f7d4d59fbc";
      fingerprint = "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5";
    };
    gaming = {
      name = "guix-gaming-games";
      url = "https://gitlab.com/guix-gaming-channels/games.git";
      introduction = "c23d64f1b8cc086659f8781b27ab6c7314c5cca5";
      fingerprint = "50F3 3E2E 5B0C 3D90 0424  ABE8 9BDC F497 A4BB CC7F";
    };
  };
in {
  config = rec {
    xdg.configFile = {
      "guix/channels.scm" = {
        text = ''(cons*
        ${pkgs.lib.concatMapStringsSep "" (channel: ''
          (channel
           (name '${channel.name})
           (url "${channel.url}")
           (introduction
             (make-channel-introduction
               "${channel.introduction}"
               (openpgp-fingerprint
                 "${channel.fingerprint}"))))
       '') (builtins.attrValues channels)}
       %default-channels)'';
      };
      "guix/home.scm" = {
        text = ''
          (use-modules (home))

          (home
            (data-directory "${config.home.homeDirectory}/data"))
        '';
      };
    };

    systemd.user.services.guix-daemon = with config.lib.guix; {
      Service = {
        ExecStart = "${pkgs.guix-ns}/bin/guix-ns-root ${homeDirectory}/gnu ${guix}/bin/guix-daemon --max-jobs=4 --debug";
        Environment = [ "GUIX_LOCPATH='${profile}/lib/locale'" ];
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
    };

    lib.guix = rec {
      inherit (config.home) homeDirectory username;
      guix = "/gnu/var/profiles/per-user/${username}/current-guix";
      profile = "/gnu/var/profiles/per-user/${username}/guix-profile";
      ns-env = pkgs.writeShellScriptBin "guix-ns-env" ''
        export GUIX_PROFILE=$(readlink -f ${homeDirectory}/${profile})
        export GUIX_LOCPATH=$GUIX_PROFILE/lib/locale
        source ${homeDirectory}/$GUIX_PROFILE/etc/profile
        exec ${pkgs.guix-ns}/bin/guix-ns ${homeDirectory}/gnu $@
      '';
      packages = {
        inherit ns-env;
        guix = pkgs.writeScriptBin "guix" ''
          #!${pkgs.execline}/bin/execlineb -S0
          ${ns-env}/bin/guix-ns-env guix $@
        '';
        jami = pkgs.writeScriptBin "jami" ''
          #!${pkgs.execline}/bin/execlineb -S0
          ${ns-env}/bin/guix-ns-env ${pkgs.dbus.lib}/bin/dbus-launch env CLUTTER_BACKEND=x11 jami $@
        '';
      };
    };

    home.packages = (builtins.attrValues config.lib.guix.packages);

    home.activation.guix-home = config.lib.dag.entryAnywhere ''
      function guixReconfigure() {
        ${config.lib.guix.packages.guix}/bin/guix home reconfigure ${config.xdg.configHome}/guix/home.scm
      }

      guixReconfigure || true
    '';
  };
}
