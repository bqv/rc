{ config, pkgs, lib, domains, ... }:

let
  cfg = config.services.hydra;
in {
  services.hydra = lib.mkIf cfg.enable {
    hydraURL = "http://localhost:3000";
    notificationSender = "hydra@${domains.home}";
    buildMachinesFiles = [];
    useSubstitutes = true;
  };
}
