{ config, pkgs, lib, domains, ... }:

let
  cfg = config.services.hydra;
in {
  disabledModules = [
    "services/continuous-integration/hydra/default.nix"
  ];

  options.services.hydra.enable = lib.mkEnableOption "Hydra services";

  config.services.hydra-dev = lib.mkIf cfg.enable {
    enable = true;
    package = pkgs.hydra-unstable.overrideAttrs (drv: {
      postUnpack = ''
        sed -i 's/restrictEval = true/restrictEval = false/' source/src/hydra-eval-jobs/hydra-eval-jobs.cc
      '';
    });
    listenHost = "0.0.0.0";
    port = 9999;
    minimumDiskFree = 20; # in GB
    minimumDiskFreeEvaluator = 5;
    hydraURL = "https://hydra.${domains.home}";
    notificationSender = "hydra@${domains.home}";
    logo = null;
    useSubstitutes = true;
    extraConfig = ''
      using_frontend_proxy 1
      max_output_size = 4294967296
      evaluator_initial_heap_size = 4294967296
    '';
  };

  config.nix.allowedUsers = lib.mkIf cfg.enable [ "@hydra" ];
}
