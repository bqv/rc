{ config, pkgs, ... }:

{
  services.traefik = {
    enable = true;

    dynamicConfigOptions = {
      http = {
	routers = {
	  ping = {
	    entryPoints = [ "http" "https" ];
	    rule = "Host(`ping.fron.io`)";
	    service = "ping@internal";
	  };
	  api = {
	    entryPoints = [ "http" "https" ];
	    rule = "Host(`traefik.fron.io`)";
	    service = "api@internal";
	    middlewares = [ "auth" ];
	    tls = {
	      domains = [
		{
		  main = "foobar";
		  sans = [ "foobar" "foobar" ];
		}
		{
		  main = "foobar";
		  sans = [ "foobar" "foobar" ];
		}
	      ];
	      options = "foobar";
	    };
	  };
	  Router1 = {
	    entryPoints = [ "foobar" "foobar" ];
	    middlewares = [ "foobar" "foobar" ];
	    priority = 42;
	    rule = "foobar";
	    service = "foobar";
	    tls = {
	      certResolver = "foobar";
	      domains = [
		{
		  main = "foobar";
		  sans = [ "foobar" "foobar" ];
		}
		{
		  main = "foobar";
		  sans = [ "foobar" "foobar" ];
		}
	      ];
	      options = "foobar";
	    };
	  };
	};

	middlewares = {
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
	 #Middleware15 = {
	 #  redirectRegex = {
	 #    permanent = true;
	 #    regex = "foobar";
	 #    replacement = "foobar";
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
	  Service01 = {
	    loadBalancer = {
	      healthCheck = {
		followRedirects = true;
		headers = {
		  name0 = "foobar";
		  name1 = "foobar";
		};
		hostname = "foobar";
		interval = "foobar";
		path = "foobar";
		port = 42;
		scheme = "foobar";
		timeout = "foobar";
	      };
	      passHostHeader = true;
	      responseForwarding = { flushInterval = "foobar"; };
	      servers = [ { url = "foobar"; } { url = "foobar"; } ];
	      sticky = {
		cookie = {
		  httpOnly = true;
		  name = "foobar";
		  sameSite = "foobar";
		  secure = true;
		};
	      };
	    };
	  };
	  Service02 = {
	    mirroring = {
	      maxBodySize = 42;
	      mirrors = [
		{
		  name = "foobar";
		  percent = 42;
		}
		{
		  name = "foobar";
		  percent = 42;
		}
	      ];
	      service = "foobar";
	    };
	  };
	  Service03 = {
	    weighted = {
	      services = [
		{
		  name = "foobar";
		  weight = 42;
		}
		{
		  name = "foobar";
		  weight = 42;
		}
	      ];
	      sticky = {
		cookie = {
		  httpOnly = true;
		  name = "foobar";
		  sameSite = "foobar";
		  secure = true;
		};
	      };
	    };
	  };
	};
      };

      tcp = {
	routers = {
	  TCPRouter0 = {
	    entryPoints = [ "foobar" "foobar" ];
	    rule = "foobar";
	    service = "foobar";
	    tls = {
	      certResolver = "foobar";
	      domains = [
		{
		  main = "foobar";
		  sans = [ "foobar" "foobar" ];
		}
		{
		  main = "foobar";
		  sans = [ "foobar" "foobar" ];
		}
	      ];
	      options = "foobar";
	      passthrough = true;
	    };
	  };
	  TCPRouter1 = {
	    entryPoints = [ "foobar" "foobar" ];
	    rule = "foobar";
	    service = "foobar";
	    tls = {
	      certResolver = "foobar";
	      domains = [
		{
		  main = "foobar";
		  sans = [ "foobar" "foobar" ];
		}
		{
		  main = "foobar";
		  sans = [ "foobar" "foobar" ];
		}
	      ];
	      options = "foobar";
	      passthrough = true;
	    };
	  };
	};
	services = {
	  TCPService01 = {
	    loadBalancer = {
	      servers = [ { address = "foobar"; } { address = "foobar"; } ];
	      terminationDelay = 42;
	    };
	  };
	  TCPService02 = {
	    weighted = {
	      services = [
		{
		  name = "foobar";
		  weight = 42;
		}
		{
		  name = "foobar";
		  weight = 42;
		}
	      ];
	    };
	  };
	};
      };

      tls = {
	certificates = [
	  {
	    certFile = "foobar";
	    keyFile = "foobar";
	    stores = [ "foobar" "foobar" ];
	  }
	  {
	    certFile = "foobar";
	    keyFile = "foobar";
	    stores = [ "foobar" "foobar" ];
	  }
	];
	options = {
	  Options0 = {
	    cipherSuites = [ "foobar" "foobar" ];
	    clientAuth = {
	      caFiles = [ "foobar" "foobar" ];
	      clientAuthType = "foobar";
	    };
	    curvePreferences = [ "foobar" "foobar" ];
	    maxVersion = "foobar";
	    minVersion = "foobar";
	    preferServerCipherSuites = true;
	    sniStrict = true;
	  };
	  Options1 = {
	    cipherSuites = [ "foobar" "foobar" ];
	    clientAuth = {
	      caFiles = [ "foobar" "foobar" ];
	      clientAuthType = "foobar";
	    };
	    curvePreferences = [ "foobar" "foobar" ];
	    maxVersion = "foobar";
	    minVersion = "foobar";
	    preferServerCipherSuites = true;
	    sniStrict = true;
	  };
	};
	stores = {
	  Store0 = {
	    defaultCertificate = {
	      certFile = "foobar";
	      keyFile = "foobar";
	    };
	  };
	  Store1 = {
	    defaultCertificate = {
	      certFile = "foobar";
	      keyFile = "foobar";
	    };
	  };
	};
      };

      udp = {
	routers = {
	  UDPRouter0 = {
	    entryPoints = [ "foobar" "foobar" ];
	    service = "foobar";
	  };
	  UDPRouter1 = {
	    entryPoints = [ "foobar" "foobar" ];
	    service = "foobar";
	  };
	};
	services = {
	  UDPService01 = {
	    loadBalancer = {
	      servers = [ { address = "foobar"; } { address = "foobar"; } ];
	    };
	  };
	  UDPService02 = {
	    weighted = {
	      services = [
		{
		  name = "foobar";
		  weight = 42;
		}
		{
		  name = "foobar";
		  weight = 42;
		}
	      ];
	    };
	  };
	};
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
	    trustedIPs = [ "127.0.0.1" "10.0.0.1/8" ];
	  };
	  http = {
	   #middlewares = [ "auth@file" "strip@file" ];
	   #tls = {
	   #  certResolver = "foobar";
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
	  proxyProtocol = {
	    insecure = true;
	    trustedIPs = [ "127.0.0.1" "10.0.0.1/8" ];
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
	streaming = {
	  address = ":1704/udp";
	};
      };

      providers = {
	providersThrottleDuration = 10;

        docker.exposedByDefault = false;
	file = {
	  debugLogGeneratedTemplate = true;
	  directory = "foobar";
	  filename = "foobar";
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
