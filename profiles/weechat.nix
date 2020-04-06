{ config, lib, pkgs, ... }:

let
  systemWeechat = pkgs.weechat.override {
    configure = { availablePlugins, ... }: {
      scripts = with pkgs.weechatScripts; [ weechat-matrix ];
      plugins = builtins.attrValues (availablePlugins // {
        python = availablePlugins.python.withPackages (ps: with ps; [
          pkgs.weechatScripts.weechat-matrix
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
