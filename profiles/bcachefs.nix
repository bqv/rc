{ config, pkgs, lib, flakes, ... }:

{
  boot.supportedFilesystems = [ "bcachefs" ];
  boot.kernelPatches = [{
    name = "bcachefs-acl";
    patch = null;
    extraConfig = ''
      BCACHEFS_POSIX_ACL y
    '';
  }];
}
