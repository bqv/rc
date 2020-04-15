{ config, lib, pkgs, ... }:

let
  weechat-matrix = pkgs.weechatScripts.weechat-matrix;
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
