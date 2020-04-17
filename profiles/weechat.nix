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
    patches = [
      (pkgs.fetchpatch {
        url = "https://github.com/poljar/matrix-nio/commit/dfb2f9d12d557aff5bb07f01ac9691d8908bbf67.patch";
        sha256 = "0x378cq71y15ri6g3fznfcg8ws4nrcfpaxcv38dzmlbizg08gwzg";
      })
    ];
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
in { # TODO: https://github.com/rycee/home-manager/pull/953
  environment.systemPackages = with pkgs; [ systemWeechat dtach ];

  systemd.services.weechat = {
    serviceConfig = {
      Type = "simple";
      Restart = "always";
      RestartSec = "30";
      User = "weechat";
      Group = "users";
      StandardInput = "tty";
      StandardOutput = "tty";
      StandardError = "journal";
      TTYPath = "/dev/tty8";
      LogsDirectory = "weechat";
      StateDirectory = "weechat";
      RuntimeDirectory = "weechat";
    };
    path = [ "/run/current-system/sw" ];
    script = with pkgs; ''
      cd $RUNTIME_DIRECTORY
      exec ${dtach}/bin/dtach -A weechat -e '^z' \
        ${systemWeechat}/bin/weechat -d $STATE_DIRECTORY -r '/set logger.file.path '$LOGS_DIRECTORY
    '';
    wantedBy = [ "multi-user.target" ];
  };

  users.users.weechat = {
    home = "/var/lib/weechat";
    group = "users";
    isSystemUser = true;
  };
}
