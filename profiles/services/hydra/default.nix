{ config, pkgs, lib, domains, ... }:

let
  cfg = config.services.hydra;
in {
  services.hydra = lib.mkIf cfg.enable {
    package = pkgs.hydra-unstable;
    listenHost = "0.0.0.0";
    port = 9999;
    minimumDiskFree = 20; # in GB
    minimumDiskFreeEvaluator = 5;
    hydraURL = "https://hydra.${domains.home}";
    notificationSender = "hydra@${domains.home}";
    logo = null;
    buildMachinesFiles = [];
    useSubstitutes = true;
    extraConfig = ''
      using_frontend_proxy 1
      max_output_size = 4294967296
      evaluator_initial_heap_size = 4294967296
    '';
  };
}
