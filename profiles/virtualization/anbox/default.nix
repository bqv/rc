{ config, pkgs, lib, ... }:

{
  config = lib.mkIf config.virtualisation.anbox.enable {
    boot.kernelPatches = [{
      name = "ashmem-binder";
      patch = null;
      extraConfig = ''
        ASHMEM y
        ANDROID y
        ANDROID_BINDER_IPC y
        ANDROID_BINDERFS y
        ANDROID_BINDER_DEVICES binder,hwbinder,vndbinder
      '';
    }];
  };
}
