{ config, lib, pkgs, domains, hosts, ... }:

with lib; let
  cfg = config.programs.ssh;
in {
  config = mkIf cfg.enable {
    programs.ssh = {
      userKnownHostsFile = "/dev/null";
      extraOptionOverrides = {
        Include = "config.*";
      };
      extraConfig = ''
        VerifyHostKeyDNS yes
        VisualHostKey yes
        #SendEnv LANG LC_*
        #Ciphers +aes128-cbc,3des-cbc,aes192-cbc
        Ciphers +aes256-cbc
      '';
      matchBlocks = {
        zeta = rec {
          host = "${hostname} zeta.${domains.home} zeta";
          hostname = hosts.wireguard.ipv4.zeta;
          user = "bao";
          extraOptions = {
            StrictHostKeyChecking = "no";
            SetEnv = "DVTM=off";
          };
        };
        theta = rec {
          host = "${hosts.wireguard.ipv4.theta} ${hostname} theta";
          hostname = hosts.lan.phi;
          user = "leaf";
          extraOptions = {
            StrictHostKeyChecking = "no";
          };
        };
        delta = rec {
          host = "${hosts.wireguard.ipv4.delta} delta";
          extraOptions = {
            StrictHostKeyChecking = "no";
          };
        };
        phi = rec {
          host = "${hosts.wireguard.ipv4.phi} ${hostname} phi";
          hostname = hosts.lan.phi;
          user = "leaf";
          extraOptions = {
            StrictHostKeyChecking = "no";
          };
        };

        epsilon = rec {
          host = "${hostname} epsilon";
          hostname = hosts.lan.epsilon;
          user = "aion";
          extraOptions = {
            StrictHostKeyChecking = "no";
          };
        };

        leo = rec {
          host = "${hostname} leo";
          hostname = hosts.lan.leo;
          user = "kani";
          extraOptions = {
            StrictHostKeyChecking = "no";
          };
        };

        dev = {
          host = "dev.${domains.home}";
          port = 5022;
          identityFile = "~/.ssh/id_rsa";
          extraOptions = {
            StrictHostKeyChecking = "no";
            HostKeyAlgorithms = "ssh-rsa,ssh-dss";
            KexAlgorithms = "+diffie-hellman-group1-sha1";
            Ciphers = "aes128-ctr,aes192-ctr,aes256-ctr,aes128-cbc,3des-cbc";
          };
        };
        localhost = {
          host = "localhost";
          forwardAgent = true;
          extraOptions = {
            AddKeysToAgent = "ask";
          };
        };

        azure = {
          host = "ssh.dev.azure.com";
          user = "git";
          identityFile = "~/.ssh/id_rsa";
          identitiesOnly = true;
          extraOptions = {
            StrictHostKeyChecking = "no";
            HostKeyAlgorithms = "ssh-rsa,ssh-dss";
          };
        };
        bitbucket = {
          host = "bitbucket.org";
          user = "git";
          identityFile = "~/.ssh/id_rsa";
          extraOptions = {
            StrictHostKeyChecking = "no";
            HostKeyAlgorithms = "ssh-rsa,ssh-dss";
          };
        };
        github = {
          host = "github.com";
          user = "git";
          identityFile = "~/.ssh/id_ecdsa";
          extraOptions = {
            StrictHostKeyChecking = "no";
            HostKeyAlgorithms = "ssh-rsa,ssh-dss";
          };
        };
        darcs = {
          host = "hub.darcs.net";
          user = "darcs";
          identityFile = "~/.ssh/id_rsa";
          extraOptions = {
            StrictHostKeyChecking = "no";
            HostKeyAlgorithms = "ssh-rsa,ssh-dss";
          };
        };
      };
    };
  };
}
