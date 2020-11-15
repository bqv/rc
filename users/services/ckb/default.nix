{ config, lib, pkgs, ... }:

with lib; let
  cfg = config.services.ckb;
in {
  options.services.ckb.enable = lib.mkEnableOption "the ckb-next application";

  config = mkIf cfg.enable rec {
    xdg.configFile."ckb-next/ckb-next.conf" = {
      text = import ./conf.nix { inherit lib; };
    };
    systemd.user.services.ckb-next = {
      Service = {
        ExecStart = "${pkgs.ckb-next}/bin/ckb-next --platform vnc:port=5995";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
    };
  };
}
