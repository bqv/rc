{ super, config, lib, pkgs, ... }:

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
        (setq aria2-rcp-secret "${super.services.aria2.rpcSecret}")
      '';
    };
  };
}
