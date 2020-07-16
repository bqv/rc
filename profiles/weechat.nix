{ config, lib, pkgs, ... }:

let
  systemWeechat = pkgs.weechat.override {
    configure = { availablePlugins, ... }: {
      scripts = [ pkgs.weechatScripts.weechat-matrix ];
      plugins = builtins.attrValues (availablePlugins // {
        python = availablePlugins.python.withPackages (ps: with ps; [
          pkgs.weechatScripts.weechat-matrix
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
     #ExecStartPost = with pkgs; ''${bash}/bin/bash -c "sleep 60 && echo -e '/allpv close' | ${dtach}/bin/dtach -p /run/weechat/weechat || true"'';
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
