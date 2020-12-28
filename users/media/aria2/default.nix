{ nixosConfig, config, lib, pkgs, ... }:

with lib; let
  cfg = config.programs.aria2p;
in {
  options = {
    programs.aria2p = {
      enable = mkEnableOption "Enable aria2p";
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      pkgs.python3Packages.aria2p
    ];

    emacs-loader.aria2 = {
      enable = lib.mkDefault false;
      config = ''
        (setq aria2-rcp-secret "${nixosConfig.services.aria2.rpcSecret}")
      '';
    };

    xdg.configFile."aria2/aria2.conf".text = ''
      dir=/srv/aria
      continue=true
      bt-save-metadata=true
      file-allocation=prealloc
      seed-ratio=0
      enable-dht=true
      enable-dht6=true
      enable-peer-exchange=true
    '';
  };
}
