{ config, lib, pkgs, domains, hosts, ... }:

with lib; let
  cfg = config.programs.ssh;
in {
  config = mkIf cfg.enable {
    programs.ssh = {
      userKnownHostsFile = "/dev/null";
      extraConfig = ''
        VerifyHostKeyDNS yes
        VisualHostKey yes
        #SendEnv LANG LC_*
        #Ciphers +aes128-cbc,3des-cbc,aes192-cbc
        Ciphers +aes256-cbc
      '';
      matchBlocks = {
        zeta = {
          host = "${hosts.wireguard.zeta} zeta.${domains.home}";
          user = "bao";
          extraOptions = {
            StrictHostKeyChecking = "no";
          };
        };
        theta = rec {
          host = "${hosts.wireguard.theta} ${hostname} theta.${domains.home}";
          hostname = hosts.lan.phi;
          user = "leaf";
          extraOptions = {
            StrictHostKeyChecking = "no";
          };
        };
        delta = {
          host = "${hosts.wireguard.delta}";
          extraOptions = {
            StrictHostKeyChecking = "no";
          };
        };
        phi = {
          host = "${hosts.wireguard.phi} ${hosts.lan.phi} phi.local";
          user = "leaf";
          extraOptions = {
            StrictHostKeyChecking = "no";
          };
        };

        leo = {
          host = "${hosts.lan.leo}";
          user = "jas";
          extraOptions = {
            StrictHostKeyChecking = "no";
            PubkeyAuthentication = "no";
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
