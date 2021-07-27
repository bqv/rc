{ config, pkgs, lib, ... }:

{
  # tell kernel to use SE Linux
  boot.kernelParams = [ "security=selinux" "selinux=1" ];

  # compile kernel with SE Linux support - but also support for other LSM modules
  boot.kernelPatches = [ {
    name = "selinux-config";
    patch = null;
    extraConfig = ''
      SECURITY_SELINUX y
      SECURITY_SELINUX_BOOTPARAM n
      SECURITY_SELINUX_DISABLE n
      SECURITY_SELINUX_DEVELOP y
      SECURITY_SELINUX_AVC_STATS y
      SECURITY_SELINUX_CHECKREQPROT_VALUE 0
      DEFAULT_SECURITY_SELINUX n
    '';
  } ];

  # policycoreutils is for load_policy, fixfiles, setfiles, setsebool, semodile, and sestatus.
  environment.systemPackages = with pkgs; [ policycoreutils ];

  environment.etc."selinux/config".text = ''
    # This file controls the state of SELinux on the system.
    # SELINUX= can take one of these three values:
    #     enforcing - SELinux security policy is enforced.
    #     permissive - SELinux prints warnings instead of enforcing.
    #     disabled - No SELinux policy is loaded.
    SELINUX=enforcing
    # SELINUXTYPE= can take one of three two values:
    #     targeted - Targeted processes are protected,
    #     minimum - Modification of targeted policy. Only selected processes are protected.
    #     mls - Multi Level Security protection.
    SELINUXTYPE=targeted
  '';

  # build systemd with SE Linux support so it loads policy at boot and supports file labelling
  systemd.package = pkgs.systemd.override { withSelinux = true; };

  security.apparmor.enable = lib.mkForce false;
}
