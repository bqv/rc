{ config, pkgs, lib, ... }:

{
  disabledModules = [ "tasks/filesystems/zfs.nix" ];

  environment.systemPackages = [ pkgs.bcachefs-tools ];

  boot.supportedFilesystems = [ "bcachefs" ];

  boot.kernelPackages = lib.mkForce pkgs.linuxPackages_testing_bcachefs;
  boot.kernelPatches = [{
    name = "bcachefs-acl";
    patch = null;
    extraConfig = ''
      BCACHEFS_POSIX_ACL y
    '';
  } {
    name = "bcachefs-debug";
    patch = null;
    extraConfig = ''
      BCACHEFS_DEBUG y
    '';
  }];
}
