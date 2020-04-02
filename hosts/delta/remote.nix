{ config, lib, pkgs, ... }:

{
  programs.mosh.enable = true;

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    knownHosts = {
      "Alpha CA" = {
        certAuthority = true;
        hostNames = [ "*" ];
        publicKeyFile = ../../secrets/delta.ssh_host-ca.pub;
      };
      "delta" = {
        hostNames = [ "localhost" "127.0.0.1" "::1" ];
        publicKeyFile = ../../secrets/delta.ssh_host_ed25519_key.pub;
      };
    };
    hostKeys = [
      { bits = 4096; path = "/etc/ssh/ssh_host_rsa_key"; type = "rsa"; }
      { path = "/etc/ssh/ssh_host_ecdsa_key"; type = "ecdsa"; }
      { path = "/etc/ssh/ssh_host_ed25519_key"; type = "ed25519"; }
    ];
    permitRootLogin = "without-password";
    extraConfig = ''
      HostCertificate /etc/ssh/ssh_host_rsa_key-cert.pub
      HostCertificate /etc/ssh/ssh_host_ecdsa_key-cert.pub
      HostCertificate /etc/ssh/ssh_host_ed25519_key-cert.pub
      TrustedUserCAKeys /etc/ssh/ssh_user-ca.pub
      RevokedKeys /etc/ssh/ssh_revoked_keys
     #PubkeyAuthentication yes
      ClientAliveInterval 120
      ClientAliveCountMax 15
     #HostKeyAlgorithms +ssh-dss
    '';
  };

  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.enableSSHSupport = true;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = let
    sshd = 22;
  in [
    sshd
  ];
  networking.firewall.allowedUDPPorts = let
    sshd = 22;
  in [
    sshd
  ];
}
