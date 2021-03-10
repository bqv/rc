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
           #middlewares = [ "auth" ];
           #tls = {
           #  domains = [
           #    {
           #      main = "foobar";
           #      sans = [ "foobar" "foobar" ];
           #    }
           #    {
           #      main = "foobar";
           #      sans = [ "foobar" "foobar" ];
           #    }
           #  ];
           #  options = "foobar";
           #};
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
            entryPoints = [ "dendrite" ];
            rule = "Host(`m.${domains.srvc}`)";
            service = "dendrite";
          };
          dendrite-https = dendrite-http // {
            entryPoints = [ "dendrite-tls" ];
            tls.domains = [{ main = "m.${domains.srvc}"; }];
          };
          construct-http = {
            entryPoints = [ "http" ];
            rule = "Host(`cs.${domains.srvc}`)";
            service = "construct";
          };
          construct-https = construct-http // {
            entryPoints = [ "https" "construct" ];
            tls.domains = [{ main = "cs.${domains.srvc}"; }];
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
         #Router1 = {
         #  entryPoints = [ "foobar" "foobar" ];
         #  middlewares = [ "foobar" "foobar" ];
         #  priority = 42;
         #  rule = "foobar";
         #  service = "foobar";
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
         #Middleware00 = { addPrefix = { prefix = "foobar"; }; };
         #Middleware01 = {
         #  basicAuth = {
         #    headerField = "foobar";
         #    realm = "foobar";
         #    removeHeader = true;
         #    users = [ "foobar" "foobar" ];
         #    usersFile = "foobar";
         #  };
         #};
         #Middleware02 = {
         #  buffering = {
         #    maxRequestBodyBytes = 42;
         #    maxResponseBodyBytes = 42;
         #    memRequestBodyBytes = 42;
         #    memResponseBodyBytes = 42;
         #    retryExpression = "foobar";
         #  };
         #};
         #Middleware03 = {
         #  chain = { middlewares = [ "foobar" "foobar" ]; };
         #};
         #Middleware04 = { circuitBreaker = { expression = "foobar"; }; };
         #Middleware05 = {
         #  compress = { excludedContentTypes = [ "foobar" "foobar" ]; };
         #};
         #Middleware06 = { contentType = { autoDetect = true; }; };
         #Middleware07 = {
         #  digestAuth = {
         #    headerField = "foobar";
         #    realm = "foobar";
         #    removeHeader = true;
         #    users = [ "foobar" "foobar" ];
         #    usersFile = "foobar";
         #  };
         #};
         #Middleware08 = {
         #  errors = {
         #    query = "foobar";
         #    service = "foobar";
         #    status = [ "foobar" "foobar" ];
         #  };
         #};
         #Middleware09 = {
         #  forwardAuth = {
         #    address = "foobar";
         #    authResponseHeaders = [ "foobar" "foobar" ];
         #    tls = {
         #      ca = "foobar";
         #      caOptional = true;
         #      cert = "foobar";
         #      insecureSkipVerify = true;
         #      key = "foobar";
         #    };
         #    trustForwardHeader = true;
         #  };
         #};
         #Middleware10 = {
         #  headers = {
         #    accessControlAllowCredentials = true;
         #    accessControlAllowHeaders = [ "foobar" "foobar" ];
         #    accessControlAllowMethods = [ "foobar" "foobar" ];
         #    accessControlAllowOrigin = "foobar";
         #    accessControlAllowOriginList = [ "foobar" "foobar" ];
         #    accessControlExposeHeaders = [ "foobar" "foobar" ];
         #    accessControlMaxAge = 42;
         #    addVaryHeader = true;
         #    allowedHosts = [ "foobar" "foobar" ];
         #    browserXssFilter = true;
         #    contentSecurityPolicy = "foobar";
         #    contentTypeNosniff = true;
         #    customBrowserXSSValue = "foobar";
         #    customFrameOptionsValue = "foobar";
         #    customRequestHeaders = {
         #      name0 = "foobar";
         #      name1 = "foobar";
         #    };
         #    customResponseHeaders = {
         #      name0 = "foobar";
         #      name1 = "foobar";
         #    };
         #    featurePolicy = "foobar";
         #    forceSTSHeader = true;
         #    frameDeny = true;
         #    hostsProxyHeaders = [ "foobar" "foobar" ];
         #    isDevelopment = true;
         #    publicKey = "foobar";
         #    referrerPolicy = "foobar";
         #    sslForceHost = true;
         #    sslHost = "foobar";
         #    sslProxyHeaders = {
         #      name0 = "foobar";
         #      name1 = "foobar";
         #    };
         #    sslRedirect = true;
         #    sslTemporaryRedirect = true;
         #    stsIncludeSubdomains = true;
         #    stsPreload = true;
         #    stsSeconds = 42;
         #  };
         #};
         #Middleware11 = {
         #  ipWhiteList = {
         #    ipStrategy = {
         #      depth = 42;
         #      excludedIPs = [ "foobar" "foobar" ];
         #    };
         #    sourceRange = [ "foobar" "foobar" ];
         #  };
         #};
         #Middleware12 = {
         #  inFlightReq = {
         #    amount = 42;
         #    sourceCriterion = {
         #      ipstrategy = {
         #        depth = 42;
         #        excludedIPs = [ "foobar" "foobar" ];
         #      };
         #      requestHeaderName = "foobar";
         #      requestHost = true;
         #    };
         #  };
         #};
         #Middleware13 = {
         #  passTLSClientCert = {
         #    info = {
         #      issuer = {
         #        commonName = true;
         #        country = true;
         #        domainComponent = true;
         #        locality = true;
         #        organization = true;
         #        province = true;
         #        serialNumber = true;
         #      };
         #      notAfter = true;
         #      notBefore = true;
         #      sans = true;
         #      serialNumber = true;
         #      subject = {
         #        commonName = true;
         #        country = true;
         #        domainComponent = true;
         #        locality = true;
         #        organization = true;
         #        province = true;
         #        serialNumber = true;
         #      };
         #    };
         #    pem = true;
         #  };
         #};
         #Middleware14 = {
         #  rateLimit = {
         #    average = 42;
         #    burst = 42;
         #    period = 42;
         #    sourceCriterion = {
         #      ipstrategy = {
         #        depth = 42;
         #        excludedIPs = [ "foobar" "foobar" ];
         #      };
         #      requestHeaderName = "foobar";
         #      requestHost = true;
         #    };
         #  };
         #};
         #Middleware16 = {
         #  redirectScheme = {
         #    permanent = true;
         #    port = "foobar";
         #    scheme = "foobar";
         #  };
         #};
         #Middleware17 = { replacePath = { path = "foobar"; }; };
         #Middleware18 = {
         #  replacePathRegex = {
         #    regex = "foobar";
         #    replacement = "foobar";
         #  };
         #};
         #Middleware19 = { retry = { attempts = 42; }; };
         #Middleware20 = {
         #  stripPrefix = {
         #    forceSlash = true;
         #    prefixes = [ "foobar" "foobar" ];
         #  };
         #};
         #Middleware21 = {
         #  stripPrefixRegex = { regex = [ "foobar" "foobar" ]; };
         #};
        };

        services = {
          auth.loadBalancer = {
           #healthCheck = {
           #  followRedirects = true;
           #  headers = {
           #    name0 = "foobar";
           #    name1 = "foobar";
           #  };
           #  hostname = "foobar";
           #  interval = "foobar";
           #  path = "foobar";
           #  port = 42;
           #  scheme = "foobar";
           #  timeout = "foobar";
           #};
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
            servers = [
              { url = "http://10.7.0.2:8008"; }
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
         #mirror-sample.mirroring = {
         #  maxBodySize = 42;
         #  mirrors = [
         #    { name = "http://127.0.0.1:8384"; percent = 42; }
         #  ];
         #  service = "foobar";
         #};
         #weighted-sample.weighted = {
         #  services = [
         #    { name = "foobar"; weight = 42; }
         #  ];
         #  sticky.cookie = {
         #    httpOnly = true;
         #    name = "foobar";
         #    sameSite = "foobar";
         #    secure = true;
         #  };
         #};
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
         # #middlewares = [ "auth@file" "strip@file" ];
         # #tls = {
         # #  certResolver = "foobar";
         # #  domains = [
         # #    {
         # #      main = "foobar";
         # #      sans = [ "foobar" "foobar" ];
         # #    }
         # #    {
         # #      main = "foobar";
         # #      sans = [ "foobar" "foobar" ];
         # #    }
         # #  ];
         # #  options = "foobar";
         # #};
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

       #docker.exposedByDefault = false;
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
        filePath = "/var/log/access";
        format = "json";
        bufferingSize = 100;
      };
    };
  };
}
