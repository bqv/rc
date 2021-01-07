{ config, pkgs, lib, ... }:

{
  boot.kernelPackages = pkgs.large.linuxPackages_testing_bcachefs;
  boot.supportedFilesystems = [ "bcachefs" ];
  boot.kernelPatches = [{
    name = "bcachefs-acl";
    patch = null;
    extraConfig = ''
      BCACHEFS_POSIX_ACL y
    '';
  }];
}
