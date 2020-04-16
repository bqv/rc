{ config, lib, pkgs, ... }:

let
  matrix-nio = pkgs.python3Packages.matrix-nio.overrideAttrs (super: rec {
    version = lib.substring 0 9 src.rev;
    src = pkgs.fetchFromGitHub {
      owner = "poljar";
      repo = "matrix-nio";
      rev = "98f0c244065ea59c2e5105bc0aab5811aea748cf";
      hash = "sha256-GQPTnGxazR3WW8WGrC4X1oXvDXPMqQ5AZxdJns87C/Q=";
    };
  });
  weechat-matrix = (pkgs.weechatScripts.weechat-matrix.override {
    inherit matrix-nio;
  }).overrideAttrs (super: rec {
    version = lib.substring 0 9 src.rev;
    src = pkgs.fetchFromGitHub {
      owner = "poljar";
      repo = "weechat-matrix";
      rev = "d415841662549f096dda09390bfdebd3ca597bac";
      hash = "sha256-QT3JNzIShaR8vlrWuGzBtLDHjn7Z6vhovcOAfttgUxo=";
    };
  });
  systemWeechat = pkgs.weechat.override {
    configure = { availablePlugins, ... }: {
      scripts = [ weechat-matrix ];
      plugins = builtins.attrValues (availablePlugins // {
        python = availablePlugins.python.withPackages (ps: with ps; [
          weechat-matrix
          websocket_client
        ]);
      });
    };
  };
in {
  environment.systemPackages = [ systemWeechat ];

  # Weechat
  systemd.user.services.weechat = {
    serviceConfig = {
      Type = "simple";
      Restart = "always";
    };
    path = [ "/run/current-system/sw" ];
    script = "exec ${systemWeechat}/bin/weechat-headless";
    wantedBy = [ "default.target" ];
  };
}
