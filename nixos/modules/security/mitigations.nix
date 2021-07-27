{ config, lib, options, ... }:
with lib;

let
  inherit (builtins) readFile fetchurl;

  cfg = config.security.mitigations;

  cmdline = lib.concatStringsSep " " ((
    if
      lib.versionOlder config.boot.kernelPackages.kernel.version "5.1.13"
    then [
      "noibrs" # We don't need no restricted indirect branch speculation
      "noibpb" # We don't need no indirect branch prediction barrier either
      "nospectre_v1" # Don't care if some program can get data from some other program when it shouldn't
      "nospectre_v2" # Don't care if some program can get data from some other program when it shouldn't
      "l1tf=off" # Why would we be flushing the L1 cache, we might need that data. So what if anyone can get at it.
      "nospec_store_bypass_disable" # Of course we want to use, not bypass, the stored data
      "no_stf_barrier" # We don't need no barriers between software, they could be friends
      "mds=off" # Zombieload attacks are fine
    ] else []) ++ [
      "mitigations=off" # Of course we don't want no mitigations
    ]);
in {
  options = {
    security.mitigations.disable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to disable spectre and meltdown mitigations in the kernel. Do
        not use this in mission critical deployments, or on any machine you do
        not have physical access to.
      '';
    };

    security.mitigations.acceptRisk = mkOption {
      type = types.bool;
      default = false;
      description = ''
        To ensure users know what they are doing, they must explicitly accept
        the risk of turning off mitigations by enabling this.
      '';
    };
  };

  config = mkIf cfg.disable {
    assertions = [{
      assertion = cfg.acceptRisk;
      message = ''
        You have enabled 'security.mitigations.disable' without accepting the
        risk of disabling mitigations.

        You must explicitly accept the risk of running the kernel without
        Spectre or Meltdown mitigations. Set 'security.mitigations.acceptRisk'
        to 'true' only if you know what your doing!
      '';
    }];

    boot.kernelParams = splitString " " cmdline;

  };
}
