{ config, lib, pkgs, usr, domains, hosts, ... }:

{
  systemd.services.traefik.serviceConfig.LimitNPROC = lib.mkForce null; # Ridiculous and broken
  users.users.traefik.extraGroups = [ "keys" ]; # For acme certificates

 #ssl.servers = lib.flatten (
 #  lib.mapAttrsToList (_: router: lib.mapAttrsToList (_: lib.id)
 #                                 router.tls.domains)
 #  config.services.traefik.dynamicConfigOptions.http.routers
 #);

  services.traefik = {
    enable = true;

    dynamicConfigOptions = {
      http = {
        routers = rec {
          ping = {
            entryPoints = [ "http" "https" ];
            rule = "Host(`ping.${domains.home}`)";
            service = "ping@internal";
          };
          api = {
            entryPoints = [ "http" "https" ];
            rule = "Host(`traefik.${domains.home}`)";
            service = "api@internal";
          };
          auth-request = {
            entryPoints = [ "http" "https" ];
            rule = "Host(`sso.${domains.home}`)";
            service = "auth";
          };
          torrent = {
            entryPoints = [ "http" "https" ];
            rule = "Host(`torrent.${domains.home}`)";
            service = "torrent";
          };
          sync = {
            entryPoints = [ "http" "https" ];
            rule = "Host(`sync.${domains.home}`)";
            service = "sync";
          };
          search-http = {
            entryPoints = [ "http" ];
            rule = "Host(`search.${domains.home}`)";
            service = "searx";
          };
          search-https = search-http // {
            entryPoints = [ "https" ];
            tls = {};
          };
          hydra-http = {
            entryPoints = [ "http" ];
            rule = "Host(`hydra.${domains.home}`)";
            service = "hydra";
          };
          hydra-https = hydra-http // {
            entryPoints = [ "https" ];
            tls = {};
          };
          yacy = {
            entryPoints = [ "yacy" ];
            rule = "Host(`yacy.${domains.home}`)";
            service = "yacy";
          };
          gpx = {
            entryPoints = [ "http" "https" ];
            rule = "Host(`gpx.${domains.home}`)";
            service = "gpx";
          };
          mastodon-http = {
            entryPoints = [ "http" ];
            rule = "Host(`u.${domains.srvc}`)";
            service = "mastodon";
          };
          mastodon-https = mastodon-http // {
            entryPoints = [ "https" ];
            tls.domains = [{ main = "u.${domains.srvc}"; }];
          };
          twitterpub-http = {
            entryPoints = [ "http" ];
            rule = "Host(`tw.${domains.srvc}`)";
            service = "twitterpub";
          };
          twitterpub-https = twitterpub-http // {
            entryPoints = [ "https" ];
            tls.domains = [{ main = "tw.${domains.srvc}"; }];
          };
          dendrite-http = {
            entryPoints = [ "http" ];
            rule = "(Host(`matrix.${domains.srvc}`) || Host(`m.${domains.srvc}`)) && PathPrefix(`/_matrix`)";
            service = "dendrite";
          };
          dendrite-https = dendrite-http // {
            entryPoints = [ "https" ];
            tls.domains = [
              { main = "matrix.${domains.srvc}"; }
              { main = "m.${domains.srvc}"; }
            ];
          };
          dendrite-http-wellknown = dendrite-http // {
            rule = "(Host(`matrix.${domains.srvc}`) || Host(`m.${domains.srvc}`) || Host(`${domains.srvc}`)) && PathPrefix(`/.well-known/matrix`)";
            service = "dendrite-wellknown";
            middlewares = [ "matrix-wellknown" "no-cors" ];
          };
          dendrite-https-wellknown = dendrite-https // {
            rule = "(Host(`matrix.${domains.srvc}`) || Host(`m.${domains.srvc}`) || Host(`${domains.srvc}`)) && PathPrefix(`/.well-known/matrix`)";
            service = "dendrite-wellknown";
            middlewares = [ "matrix-wellknown" "no-cors" ];
          };
          certauth = {
            entryPoints = [ "http" "https" ];
            rule = "Host(`ca.${domains.home}`)";
            service = "certauth";
          };
          anki = {
            entryPoints = [ "http" "https" "anki" ];
            rule = "Host(`anki.${domains.home}`)";
            service = "anki";
          };
          klaus-http = {
            entryPoints = [ "http" ];
            rule = "Host(`dev.${domains.home}`) || Host(`rc.${domains.home}`)";
            service = "klaus";
            middlewares = [ "redirect-nixrc" ];
          };
          klaus-https = klaus-http // {
            entryPoints = [ "https" ];
            tls.domains = [
              { main = "dev.${domains.home}"; }
              { main = "rc.${domains.home}"; }
            ];
          };
          ipfs-http = {
            entryPoints = [ "http" ];
            rule = "Host(`ipfs.${domains.home}`) && PathPrefix(`/ipfs`)" +
            " || PathPrefix(`/ipns`) || PathPrefix(`/ipld`)";
            service = "ipfs";
          };
          ipfs-https = ipfs-http // {
            entryPoints = [ "https" ];
            tls.domains = [
              { main = "ipfs.${domains.home}"; }
            ];
          };
          jellyfin-http = {
            entryPoints = [ "http" "jellyfin" ];
            rule = "Host(`media.${domains.home}`)";
            service = "jellyfin";
          };
          jellyfin-https = jellyfin-http // {
            entryPoints = [ "https" "jellyfin-tls" ];
            tls.domains = [
              { main = "media.${domains.home}"; }
            ];
          };
          transmission-http = {
            entryPoints = [ "http" "transmission-rpc" ];
            rule = "Host(`torrent.${domains.home}`)" +
            " || (Host(`media.${domains.home}`) && PathPrefix(`/transmission`))";
            service = "transmission";
          };
          transmission-https = transmission-http // {
            entryPoints = [ "https" ];
            tls.domains = [
              { main = "torrent.${domains.home}"; }
            ];
          };
          sonarr-http = {
            entryPoints = [ "http" ];
            rule = "Host(`media.${domains.home}`) && PathPrefix(`/sonarr`)";
            service = "sonarr";
          };
          sonarr-https = sonarr-http // {
            entryPoints = [ "https" ];
            tls.domains = [
              { main = "media.${domains.home}"; }
            ];
          };
          radarr-http = {
            entryPoints = [ "http" ];
            rule = "Host(`media.${domains.home}`) && PathPrefix(`/radarr`)";
            service = "radarr";
          };
          radarr-https = radarr-http // {
            entryPoints = [ "https" ];
            tls.domains = [
              { main = "media.${domains.home}"; }
            ];
          };
          jackett-http = {
            entryPoints = [ "http" ];
            rule = "Host(`media.${domains.home}`) && PathPrefix(`/jackett`)";
            service = "jackett";
          };
          jackett-https = jackett-http // {
            entryPoints = [ "https" ];
            tls.domains = [
              { main = "media.${domains.home}"; }
            ];
          };
          lidarr-http = {
            entryPoints = [ "http" ];
            rule = "Host(`media.${domains.home}`) && PathPrefix(`/lidarr`)";
            service = "lidarr";
          };
          lidarr-https = lidarr-http // {
            entryPoints = [ "https" ];
            tls.domains = [
              { main = "media.${domains.home}"; }
            ];
          };
          bazarr-http = {
            entryPoints = [ "http" ];
            rule = "Host(`media.${domains.home}`) && PathPrefix(`/bazarr`)";
            service = "bazarr";
          };
          bazarr-https = bazarr-http // {
            entryPoints = [ "https" ];
            tls.domains = [
              { main = "media.${domains.home}"; }
            ];
          };
        };

        middlewares = {
          redirect-nixrc = {
            redirectRegex = let
              gitcreds = usr.secrets.git.github;
            in {
              permanent = false;
              regex = "^(https?)://rc.${domains.home}/(.*)";
              replacement = "\${1}://dev.${domains.home}/nixrc/\${2}";
            };
          };
          matrix-wellknown = {
            stripPrefix.prefixes = [ "/.well-known/matrix" ];
          };
          no-cors = {
            headers.accesscontrolalloworigin = "*";
          };
        };

        services = {
          auth.loadBalancer = {
            passHostHeader = true;
            responseForwarding = { flushInterval = "100ms"; };
            servers = [
              { url = "http://10.1.0.2:4010/auth"; }
            ];
          };
          torrent.loadBalancer = {
            servers = [
              { url = "http://10.1.0.2:3000"; }
            ];
          };
          sync.loadBalancer = {
            servers = [
              { url = "http://127.0.0.1:8384"; }
            ];
          };
          searx.loadBalancer = {
            servers = [
              { url = "http://${hosts.wireguard.ipv4.delta}:8888"; }
            ];
          };
          hydra.loadBalancer = {
            servers = [
              { url = "http://${hosts.wireguard.ipv4.delta}:9999"; }
            ];
          };
          yacy.loadBalancer = {
            servers = [
              { url = "http://10.5.0.2:8090"; }
            ];
          };
          gpx.loadBalancer = {
            servers = [
              { url = "http://10.1.0.2:2777"; }
            ];
          };
          mastodon.loadBalancer = {
            servers = [
              { url = "https://10.6.0.2:8443"; }
            ];
          };
          twitterpub.loadBalancer = {
            servers = [
              { url = "https://10.6.0.2:443"; }
            ];
          };
          dendrite.loadBalancer = {
            passHostHeader = true;
            servers = [
              { url = "http://10.7.0.2:8008"; }
            ];
          };
          dendrite-wellknown.loadBalancer = {
            servers = [
              { url = "http://10.7.0.2:80"; }
            ];
          };
          construct.loadBalancer = {
            servers = [
              { url = "https://10.7.0.2:4004"; }
            ];
          };
          certauth.loadBalancer = {
            servers = [
              { url = "https://10.4.0.2:443"; }
            ];
          };
          anki.loadBalancer = {
            servers = [
              { url = "http://10.9.0.2:27701"; }
            ];
          };
          klaus.loadBalancer = {
            passHostHeader = true;
            servers = [
              { url = "http://10.10.0.2:3000"; }
            ];
          };
          ipfs.loadBalancer = {
            servers = [
              { url = "http://${hosts.ipv4.zeta.address}:${lib.last (
                lib.splitString "/" config.services.ipfs.gatewayAddress)}"; }
            ];
          };
          jellyfin.loadBalancer = {
            passHostHeader = true;
            servers = [
              { url = "http://10.11.0.2:8096"; }
            ];
          };
          transmission.loadBalancer = {
            passHostHeader = true;
            servers = [
              { url = "http://10.11.0.2:9091"; }
            ];
          };
          sonarr.loadBalancer = {
            passHostHeader = true;
            servers = [
              { url = "http://10.11.0.2:8989"; }
            ];
          };
          radarr.loadBalancer = {
            passHostHeader = true;
            servers = [
              { url = "http://10.11.0.2:7878"; }
            ];
          };
          jackett.loadBalancer = {
            passHostHeader = true;
            servers = [
              { url = "http://10.11.0.2:9117"; }
            ];
          };
          lidarr.loadBalancer = {
            passHostHeader = true;
            servers = [
              { url = "http://10.11.0.2:8686"; }
            ];
          };
          bazarr.loadBalancer = {
            passHostHeader = true;
            servers = [
              { url = "http://10.11.0.2:6767"; }
            ];
          };
        };
      };

      tcp = {
        routers = {
         #ssh = {
         #  entryPoints = [ "ssh" ];
         #  rule = "HostSNI(`*`)";
         #  service = "ssh";
         #  tls = {
         #    passthrough = true;
         #  };
         #};
          smtp = {
            entryPoints = [ "smtp" ];
            rule = "HostSNI(`*`)";
            service = "smtp";
          };
          imap = {
            entryPoints = [ "imap" ];
            rule = "HostSNI(`*`)";
            service = "imap";
          };
          gitssh = {
            entryPoints = [ "ssh-alt" ];
            rule = "HostSNI(`*`)";
            service = "klaus";
          };
          dendrite = {
            entryPoints = [ "dendrite" ];
            rule = "HostSNI(`*`)";
            service = "dendrite";
          };
          dendrite-tls = {
            entryPoints = [ "dendrite-tls" ];
            rule = "HostSNI(`*`)";
            service = "dendrite-tls";
          };
          transmission-dht-tcp = {
            entryPoints = [ "transmission-dht-tcp" ];
            rule = "HostSNI(`*`)";
            service = "transmission-dht";
          };
         #irc = {
         #  entryPoints = [ "ircs" ];
         #  rule = "HostSNI(`*`)";
         #  service = "irc";
         #  tls = {
         #    passthrough = true;
         #  };
         #};
        };
        services = {
          ssh.loadBalancer = {
            servers = [
              { address = "${hosts.ipv4.zeta.address}:22"; }
            ];
            terminationDelay = 100;
          };
          smtp.loadBalancer = {
            servers = [
              { address = "10.8.0.2:1025"; }
            ];
            terminationDelay = 100;
          };
          imap.loadBalancer = {
            servers = [
              { address = "10.8.0.2:1143"; }
            ];
            terminationDelay = 100;
          };
          klaus.loadBalancer = {
            servers = [
              { address = "10.10.0.2:5022"; }
            ];
            terminationDelay = 100;
          };
          irc.loadBalancer = {
            servers = [
              { address = "${hosts.wireguard.ipv4.delta}:6697"; }
            ];
            terminationDelay = 100;
          };
         #dendrite.loadBalancer = {
         #  servers = [
         #    { address = "10.7.0.2:8008"; }
         #  ];
         #  terminationDelay = 100;
         #};
         #dendrite-tls.loadBalancer = {
         #  servers = [
         #    { address = "10.7.0.2:8448"; }
         #  ];
         #  terminationDelay = 100;
         #};
          transmission-dht.loadBalancer = {
            servers = [
              { address = "10.11.0.2:51413"; }
            ];
            terminationDelay = 100;
          };
         #weighted-sample.weighted = {
         #  services = [
         #    {
         #      name = "foobar";
         #      weight = 42;
         #    }
         #  ];
         #};
        };
      };

      udp = {
        routers = {
          transmission-dht = {
            entryPoints = [ "transmission-dht-udp" ];
            service = "transmission-dht";
          };
        };
        services = {
          transmission-dht.loadBalancer = {
            servers = [
              { address = "10.11.0.2:51413"; }
            ];
          };
        };
      };

      tls = with config.security.acme; {
        certificates = lib.mapAttrsToList (_: { directory, ... }: {
          certFile = "${directory}/cert.pem";
          keyFile = "${directory}/key.pem";
          #stores = [ "default" ];
        }) certs;
        options = {
          default = {
           #minVersion = "VersionTLS12";
           #maxVersion = "VersionTLS13";
           #cipherSuites = [ "TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256" ];
           #curvePreferences = [ "CurveP521", "CurveP384" ];
           #sniStrict = true;
           #preferServerCipherSuites = true;
            clientAuth = {
              clientAuthType = "RequestClientCert";
             #caFiles = [ "clientCA.crt" ]; # PEM files
            };
          };
         #hardened = {
         #  cipherSuites = [ "foobar" "foobar" ];
         #  clientAuth = {
         #    caFiles = [ "foobar" "foobar" ];
         #    clientAuthType = "foobar";
         #  };
         #  curvePreferences = [ "foobar" "foobar" ];
         #  maxVersion = "foobar";
         #  minVersion = "foobar";
         #  preferServerCipherSuites = true;
         #  sniStrict = true;
         #};
        };
       #stores = {
       #  default = {
       #    defaultCertificate = {
       #      certFile = "foobar";
       #      keyFile = "foobar";
       #    };
       #  };
       #};
      };
    };

    staticConfigOptions = {
      global = {
        checkNewVersion = false;
        sendAnonymousUsage = false;
      };

      serversTransport = {
        insecureSkipVerify = true;
        rootCAs = [ ];
        maxIdleConnsPerHost = 7;
        forwardingTimeouts = {
          dialTimeout = 30;
          responseHeaderTimeout = 600;
          idleConnTimeout = 90;
        };
      };

      entryPoints = {
        http = {
          address = ":80/tcp";
        };
        https = {
          address = ":443/tcp";
          forwardedHeaders = {
            insecure = true;
            trustedIPs = [ "127.0.0.1" "${hosts.wireguard.ipv4.zeta}/8" ];
          };
         #http = {
         #  middlewares = [ "auth@file" "strip@file" ];
         #  tls = {
         #    certResolver = "foobar";
         #    domains = [
         #      {
         #        main = "foobar";
         #        sans = [ "foobar" "foobar" ];
         #      }
         #      {
         #        main = "foobar";
         #        sans = [ "foobar" "foobar" ];
         #      }
         #    ];
         #    options = "foobar";
         #  };
         #};
          proxyProtocol = {
            insecure = true;
            trustedIPs = [ "127.0.0.1" "${hosts.wireguard.ipv4.zeta}/8" ];
          };
          transport = {
            lifeCycle = {
              requestAcceptGraceTimeout = 0;
              graceTimeOut = 5;
            };
            respondingTimeouts = {
              readTimeout = 0;
              writeTimeout = 0;
              idleTimeout = 180;
            };
          };
        };
       #ssh = {
       #  address = "${hosts.ipv4.zeta}:22/tcp";
       #};
        smtp = {
          address = ":1025/tcp";
        };
        imap = {
          address = ":1143/tcp";
        };
        construct = {
          address = ":4004/tcp";
        };
        ssh-alt = {
          address = ":5022/tcp";
        };
        transmission-rpc = {
          address = ":9091/tcp";
        };
        irc = {
          address = ":6667/tcp";
        };
        ircs = {
          address = ":6697/tcp";
        };
        yacy = {
          address = ":8090/tcp";
        };
        dendrite = {
          address = ":8008/tcp";
        };
        dendrite-tls = {
          address = ":8448/tcp";
        };
        jellyfin = {
          address = ":8096/tcp";
        };
        jellyfin-tls = {
          address = ":8920/tcp";
        };
        anki = {
          address = ":27701/tcp";
        };
        transmission-dht-tcp = {
          address = ":51413/tcp";
        };
        transmission-dht-udp = {
          address = ":51413/udp";
        };
      };

      providers = {
        providersThrottleDuration = 10;

        file = {
          debugLogGeneratedTemplate = true;
         #directory = "foobar";
         #filename = "foobar";
          watch = true;
        };
      };

      api = {
        dashboard = true;
        debug = true;
        insecure = true;
      };

      ping = {
        manualRouting = true;
      };

      accessLog = {
        filePath = "/var/log/traefik/access.json";
        format = "json";
        fields.headers.defaultMode = "keep";
        bufferingSize = 100;
      };
    };
  };

  services.logrotate = {
    enable = true;
    paths.traefik = {
      enable = true;
      path = "/var/log/traefik/access.*";
      user = config.systemd.services.traefik.serviceConfig.User;
      group = config.systemd.services.traefik.serviceConfig.Group;
      frequency = "daily";
      keep = 16;
    };
  };
}
