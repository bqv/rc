{
  description = "A highly structured configuration database.";

  inputs = {
    master.url = "github:nixos/nixpkgs/master";                        #|
    stable.url = "github:nixos/nixpkgs/nixos-20.03";                   #|\
    staged.url = "github:nixos/nixpkgs/staging";                       #| -- Nixpkgs
    small.url  = "github:nixos/nixpkgs/nixos-unstable-small";          #|/
    large.url  = "github:nixos/nixpkgs/nixos-unstable";                #|
    pr75800.url = "github:ma27/nixpkgs/declarative-networks-with-iwd"; #|
    pr93457.url = "github:ju1m/nixpkgs/apparmor";                      #|
    pr93659.url = "github:ju1m/nixpkgs/security.pass";                 #|

    lg531.url = "github:bqv/nixrc/54fee446d8110f516f946a5cb6a27a760e538700"; # Historical sys-531
    lg400.url = "github:bqv/nixrc/c66055501d4ef83d5e392f29d4e951b1a8289668"; # Historical sys-400

    nix.url = "github:nixos/nix/master";   #|- Nix
    nix.inputs.nixpkgs.follows = "master"; #|

    dwarffs.url = "github:edolstra/dwarffs";   #|- Dwarffs
    dwarffs.inputs.nix.follows = "nix";        #|
    dwarffs.inputs.nixpkgs.follows = "master"; #|

    home.url = "github:rycee/home-manager"; #|- Home-manager
    home.inputs.nixpkgs.follows = "large";  #|

    naersk.url = "github:nmattia/naersk";    #|- Naersk
    naersk.inputs.nixpkgs.follows = "large"; #|

    xontribs.url = "github:bqv/xontribs";      #|- Xontribs
    xontribs.inputs.nixpkgs.follows = "large"; #|

    guix.url = "github:bqv/guix";          #|- Guix
    guix.inputs.nixpkgs.follows = "large"; #|

    construct.url = "github:matrix-construct/construct"; #|- Construct
    construct.inputs.nixpkgs.follows = "large";          #|

    nix-ipfs.url = "github:obsidiansystems/nix/ipfs-develop"; #|- NixIPFS
    nix-ipfs.inputs.nixpkgs.follows = "master";               #|

    emacs.url = "github:nix-community/emacs-overlay";   # Emacs-overlay

    nyxt = { url = "github:atlas-engineer/nyxt"; flake = false; };       #|- Nyxt
    cl-webkit = { url = "github:joachifm/cl-webkit"; flake = false; };   #|  | cl-webkit
    cluffer = { url = "github:robert-strandh/cluffer"; flake = false; }; #|  | cluffer

    wayland.url = "github:colemickens/nixpkgs-wayland"; #|- Nixpkgs-wayland
    wayland.inputs.nixpkgs.follows = "large";           #|

    haskell.url = "github:input-output-hk/haskell.nix"; # Haskell.nix
    utils.url = "github:numtide/flake-utils";           # Flake-utils
    hardware.url = "github:nixos/nixos-hardware";       # Nixos-hardware

    nixus = { url = "github:infinisil/nixus"; flake = false; };                   # Nixus
    impermanence = { url = "github:nix-community/impermanence"; flake = false; }; # Impermanence
    mozilla = { url = "github:mozilla/nixpkgs-mozilla"; flake = false; };         # Nixpkgs-mozilla
    baduk = { url = "github:dustinlacewell/baduk.nix"; flake = false; };          # Baduk
    snack = { url = "github:nmattia/snack"; flake = false; };                     # Snack
    napalm = { url = "github:nmattia/napalm"; flake = false; };                   # Napalm

  };

  outputs = inputs: with builtins; let
    allSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
    diffTrace = left: right: string: value: if left != right then trace string value else value;

    # Nixos Config
    config = {
      allowUnfree = true;
      android_sdk.accept_license = true;
    };

    channels = with inputs; {
      pkgs = large;       # For packages
      modules = pr93457;  # For nixos modules
      lib = master;       # For flake-wide lib
    }; inherit (channels.lib) lib; # this ^

    # Fetch PR prepatched nixpkgs by id and hash
    fetchPullRequestForSystem = system: args@{ id, rev ? null, sha256 ? lib.fakeSha256, ... }:
      mapAttrs (k: v: trace "pkgs.${k} pinned to nixpks/pull/${toString id}" v)
        (import (builtins.fetchTarball {
          name = "nixpkgs-pull-request-${toString id}"; inherit sha256;
          url = if ! builtins.isNull rev
                then "https://github.com/NixOS/nixpkgs/archive/${rev}.zip"
                else "https://github.com/NixOS/nixpkgs/archive/pull/${toString id}/head.zip";
        }) {
          inherit system config;
          overlays = attrValues inputs.self.overlays;
        } // (removeAttrs args [ "id" "rev" "hash" ]));

    # Nonstandard channel wrapper for build visibility
    channelToOverlay = { system, config, flake, branch }: (final: prev: { ${flake} =
      mapAttrs (k: v: diffTrace (baseNameOf inputs.${flake}) (baseNameOf prev.path) "pkgs.${k} pinned to nixpkgs/${branch}" v)
      (import inputs.${flake} { inherit config system; overlays = []; });
    });
    # Loopback flake wrapper for build visibility
    flakeToOverlay = { system, flake, name }: (final: prev: { ${flake} =
      mapAttrs (k: v: diffTrace (baseNameOf inputs.${flake}) (baseNameOf prev.path) "pkgs.${k} pinned to ${name}" v)
      inputs.${flake}.legacyPackages.${system};
    });

    # Packages for nixos configs
    pkgsForSystem = system: import channels.pkgs rec {
      inherit system config;
      overlays = (attrValues inputs.self.overlays) ++ [
        (channelToOverlay { inherit system config; flake = "master"; branch = "master"; })
        (channelToOverlay { inherit system config; flake = "stable"; branch = "nixos-20.03"; })
        (channelToOverlay { inherit system config; flake = "staged"; branch = "staging"; })
        (channelToOverlay { inherit system config; flake = "small"; branch = "nixos-unstable-small"; })
        (channelToOverlay { inherit system config; flake = "large"; branch = "nixos-unstable"; })
        (final: prev: { broken = import channels.pkgs {
          inherit system overlays;
          config = config // { allowBroken = true; };
        }; })
        (flakeToOverlay { inherit system; flake = "lg531"; name = "delta/system-531-link"; })
        (flakeToOverlay { inherit system; flake = "lg400"; name = "delta/system-400-link"; })
        (import inputs.mozilla)
        (pkgs: raw: {
          inherit raw;
          naersk = inputs.naersk.lib.${system};
          snack = pkgs.callPackage (import "${inputs.snack}/snack-lib");
          napalm = pkgs.callPackage inputs.napalm;
          inherit (inputs.haskell.legacyPackages.${system}) haskell-nix; # ignore overlay, we want cache hits
        })
        (final: prev: { nix-ipfs = (inputs.nix-ipfs.overlay final prev).nix; })
        inputs.nix.overlay
        inputs.guix.overlay
        inputs.construct.overlay (final: prev: {
          riot-web = final.element-web;
          matrix-construct = (final.callPackage "${inputs.construct}/default.nix" { pkgs = final; }).overrideAttrs (_: {
            EXTRA_CXXFLAGS = "-mabm -mbmi";
            patchPhase = '' sed '/RB_INC_EXECUTION/d' -i ./include/ircd/stdinc.h '';
            preAutoreconf = let
              VERSION_COMMIT_CMD = "git rev-parse --short HEAD";
              VERSION_BRANCH_CMD = "git rev-parse --abbrev-ref HEAD";
              VERSION_TAG_CMD = "git describe --tags --abbrev=0 --dirty --always --broken";
              VERSION_CMD = "git describe --tags --always";
            in ''
              substituteInPlace configure.ac --replace "${VERSION_COMMIT_CMD}" "echo ${inputs.construct.rev}"
              substituteInPlace configure.ac --replace "${VERSION_BRANCH_CMD}" "echo master"
              substituteInPlace configure.ac --replace "${VERSION_TAG_CMD}" "echo ${inputs.construct.rev}"
              substituteInPlace configure.ac --replace "${VERSION_CMD}" "echo ${inputs.construct.rev}"
            '';
            src = builtins.toPath "${inputs.construct}/.";
          });
        })
        inputs.emacs.overlay
        inputs.xontribs.overlay
        inputs.wayland.overlay
        inputs.self.overlay
        (pkgs: lib.const {
          inherit (inputs.stable.legacyPackages.${system}) firefox thunderbird; # slow
          graalvm8 = builtins.trace "graalvm8: suspended - too big and not cached" pkgs.hello;
          inherit (inputs.pr93457.legacyPackages.${system}) apparmor apparmor-utils lvm2; # pullreq
          inherit (pkgs.lg531) nheko; # broken?
          inherit (pkgs.lg400) catt; # broken?
          epsxe = (import inputs.master {
            inherit system;
            config.allowUnfree = true;
            config.permittedInsecurePackages = [ "openssl-1.0.2u" ];
          }).epsxe.overrideAttrs ({ installPhase, ... }: {
            installPhase = let
              bios = pkgs.fetchurl {
                url = "https://ps1emulator.com/SCPH1001.BIN";
                hash = "sha256-ca+U0eR6aMEej9ufg2gEBgFRSkKlo5nNpIx9O/8emdM=";
              };
            in installPhase + ''
              mkdir -p $out/share/bios/
              ln -s ${bios} $out/share/bios/scph1001.bin
            '';
          });
        })
        (final: prev: {
          nyxt = prev.nyxt.override {
            src = final.runCommand "nyxt-source" rec {
              inherit (final.lispPackages) quicklisp;
            } ''
              mkdir $out && cd $out && cp -r ${inputs.nyxt}/* .

              chmod a+w quicklisp-client quicklisp-libraries
              rm -rf quicklisp-client quicklisp-libraries

              mkdir quicklisp-client
              cp -r $quicklisp/lib/common-lisp/quicklisp/* quicklisp-client/

              mkdir quicklisp-libraries
              ln -s ${inputs.cl-webkit} quicklisp-libraries/cl-webkit
              ln -s ${inputs.cluffer} quicklisp-libraries/cluffer
            '';
            version = let
              rev = lib.substring 0 8 inputs.nyxt.rev;
              date = lib.substring 0 8 (inputs.nyxt.lastModifiedDate or inputs.nyxt.lastModified);
            in "${date}.${rev}";
          };
          vervis = let
            inherit (final) fetchdarcs fetchgit;
            inherit (final.haskell.lib) dontHaddock unmarkBroken dontCheck doJailbreak;
            inherit (final.haskell.lib) addBuildDepend overrideCabal addBuildTool;
            haskellPackages = final.haskell.packages.ghc865.override { overrides = _: super: {
              base-noprelude = super.callHackage "base-noprelude" "4.12.0.0" {};

              git = unmarkBroken super.git; # not broken

              network = dontCheck (haskellPackages.callHackage "network" "2.6.3.6" {});
              smtp-mail = haskellPackages.callHackage "smtp-mail" "0.1.4.6" {};
              mime-mail = haskellPackages.callHackage "mime-mail" "0.4.14" {};

              persistent = haskellPackages.callHackage "persistent" "2.9.2" {};
              persistent-template = haskellPackages.callHackage "persistent-template" "2.5.4" {};
              persistent-postgresql = haskellPackages.callHackage "persistent-postgresql" "2.9.1" {};
              persistent-sqlite = haskellPackages.callHackage "persistent-sqlite" "2.9.3" { inherit (final) sqlite; };
              esqueleto = dontHaddock (dontCheck (haskellPackages.callHackage "esqueleto" "2.6.0" {}));
              persistent-mysql = haskellPackages.callHackage "persistent-mysql" "2.9.0" {};
            }; };
            cabal2nix = name: fetcher: attrs: haskellPackages.callCabal2nix name (fetcher attrs);
            deps = rec {
              darcs-lights = dontHaddock (cabal2nix "darcs-lights" fetchdarcs {
                url = "https://dev.angeley.es/s/fr33domlover/r/darcs-lights";
               #rev = "95b0fe34c220ed487885f6fbd99b40f06b9a451e";
                sha256 = "zvbB8AJYh1KEJiZuKxdNlEv5BmjMutZOElRm9QbKAqA=";
              } {});
              darcs-rev = cabal2nix "darcs-rev" fetchdarcs {
                url = "https://dev.angeley.es/s/fr33domlover/r/darcs-rev";
               #rev = "2fdbbc08134864415fc7b7ac3d38a20ec13bc283";
                sha256 = "vK/SfCOZS/EP2HOYh+6IvFpeYxMJS//7s/0NWLcLu6E=";
              } { inherit darcs-lights; };
              dvara = cabal2nix "dvara" fetchdarcs {
                url = "https://dev.angeley.es/s/fr33domlover/r/dvara";
               #rev = "e5f530ae995f2afc6f767d7c60452603f98a9545";
                sha256 = "mN0QOjSk8RgYr/YoaYVR1l9nFnpw8bKya/yD6uhc6vk=";
              } { inherit persistent-migration; };
              hit-graph = cabal2nix "hit-graph" fetchdarcs {
                url = "https://dev.angeley.es/s/fr33domlover/r/hit-graph";
               #rev = "0000000000000000000000000000000000000000";
                sha256 = "rmeDj6/kEIKLdcTbXDkcNtxz7kuUrelmKUu21mV3cX4=";
              } { inherit hit-harder; };
              hit-harder = cabal2nix "hit-harder" fetchdarcs {
                url = "https://dev.angeley.es/s/fr33domlover/r/hit-harder";
               #rev = "10275fce276792310e72c241471c60c7f4d9ffcc";
                sha256 = "ROs3hotBk1jSzDedZvU0nV77hUVua9veR01UVzPL6Cs=";
              } {};
              hit-network = dontHaddock (cabal2nix "hit-network" fetchdarcs {
                url = "https://dev.angeley.es/s/fr33domlover/r/hit-network";
               #rev = "9a8d64173e962a72427cb82e71a045c9e45a4027";
                sha256 = "zcKDDYNyPnY7e91LUC3yf/hMZOpDbOPJkY5C4u2leus=";
              } { inherit hit-graph hit-harder; });
              http-client-signature = cabal2nix "http-client-signature" fetchdarcs {
                url = "https://dev.angeley.es/s/fr33domlover/r/http-client-signature";
               #rev = "65c7c3d01d7f668479d7358263b825556a0e4da1";
                sha256 = "27VsnCz8RdiCQC8u+OEfSxfmg5du8OxwXp6uUK14l3w=";
              } { inherit http-signature; };
              http-signature = dontHaddock (cabal2nix "http-signature" fetchdarcs {
                url = "https://dev.angeley.es/s/fr33domlover/r/http-signature";
               #rev = "5617d1edf2daf5e084309f5191614e73664d3193";
                sha256 = "jnWIxwle3DgGfDMcorCd/Hy73EUcSgmU/jz8T2spOFM=";
              } {});
              persistent-email-address = cabal2nix "persistent-email-address" fetchdarcs {
                url = "https://dev.angeley.es/s/fr33domlover/r/persistent-email-address";
               #rev = "ae8c34f820be98607379ae16b1f17b11bf06e8c2";
                sha256 = "wDOi2IQk8c/724xcG8vRW53iL0Y+2j7HYSMpBTPEQa8=";
              } {};
              persistent-graph = cabal2nix "persistent-graph" fetchdarcs {
                url = "https://dev.angeley.es/s/fr33domlover/r/persistent-graph";
               #rev = "6e37c7b24d631d9883de5ab331d9334e50ff816e";
                sha256 = "DGQtxjoYkDNesWD5yrhoBnFcRB8poZtHXReYaZwLDwg=";
              } {};
              persistent-migration = cabal2nix "persistent-migration" fetchdarcs {
                url = "https://dev.angeley.es/s/fr33domlover/r/persistent-migration";
               #rev = "0000000000000000000000000000000000000000";
                sha256 = "a2ezrpQT0gxRQ/v81sKNKvg6EsYKF+Uc/w09oMSodM8=";
              } {};
              time-interval-aeson = cabal2nix "time-interval-aeson" fetchdarcs {
                url = "https://dev.angeley.es/s/fr33domlover/r/time-interval-aeson";
               #rev = "15919637951f7276e27863f879d646bd46734965";
                sha256 = "gXjWIC/5Y3vGb7pNEVj/dcNpOxRYi+umlPYMfHfjdXg=";
              } {};
              ssh = doJailbreak (cabal2nix "ssh" fetchdarcs {
                url = "https://dev.angeley.es/s/fr33domlover/r/ssh";
               #rev = "0000000000000000000000000000000000000000";
                sha256 = "+V3uW4REku8J5ZyjWKsPPnzf75atvi0KhPOU6DWEYt4=";
              } {});
             #} rec {
             #  HsOpenSSL = haskellPackages.HsOpenSSL.override { inherit network; };
             #  network = dontCheck (haskellPackages.callHackage "network" "2.6.3.6" {});
             #});
              yesod-auth-account = dontCheck (cabal2nix "yesod-auth-account" fetchgit {
                url = "https://dev.angeley.es/s/fr33domlover/r/yesod-auth-account";
               #rev = "0000000000000000000000000000000000000000";
                sha256 = "7nA/j2cIBUrNlNZ4Zdm1ZxMJ729PkuygJqOmVbcL+Ac=";
              } { inherit persistent-email-address; });
              yesod-http-signature = cabal2nix "yesod-http-signature" fetchdarcs {
                url = "https://dev.angeley.es/s/fr33domlover/r/yesod-http-signature";
               #rev = "60cbae6c92aaa0bd544bf05be7ba9391156e4b60";
                sha256 = "kvPOZHTrsY9+fE8cZ+JUVti1P9pFjgxK8+QTRh8jrVk=";
              } { inherit http-client-signature http-signature; };
              yesod-mail-send = cabal2nix "yesod-mail-send" fetchdarcs {
                url = "https://dev.angeley.es/s/fr33domlover/r/yesod-mail-send";
               #rev = "95d0657cd8f25036e57659cd157ddafcc74dc2d3";
                sha256 = "OlAphEd/x6kzYPAMHNvwXvDDs/5G4GJudc/+FWxyZQc=";
              } {};
            };
            vervis = overrideCabal (doJailbreak (cabal2nix "vervis" fetchdarcs {
              url = "https://dev.angeley.es/s/fr33domlover/r/vervis";
             #rev = "eb7a1c26e489dd8ab8f6abc2a68b53278ccc2243";
              sha256 = "hcovmtTHkxsQBN+PdoBHDWxpT1xfDjDewhKFZZxKU88=";
            } deps)) (drv: {
              preBuild = '' sed -i 's|\$localInstallRoot|"'$out'"|g' src/Vervis/Settings.hs '';
            });
            vervisPkgs = final.haskell-nix.project {
              src = final.runCommand "vervis" {
                vervis = vervis.src;
                darcs_lights = deps.darcs-lights.src;
                darcs_rev = deps.darcs-rev.src;
                dvara = deps.dvara.src;
                hit_graph = deps.hit-graph.src;
                hit_harder = deps.hit-harder.src;
                hit_network = deps.hit-network.src;
                http_client_signature = deps.http-client-signature.src;
                http_signature = deps.http-signature.src;
                persistent_email_address = deps.persistent-email-address.src;
                persistent_graph = deps.persistent-graph.src;
                persistent_migration = deps.persistent-migration.src;
                time_interval_aeson = deps.time-interval-aeson.src;
                ssh = deps.ssh.src;
                yesod_auth_account = deps.yesod-auth-account.src;
                yesod_http_signature = deps.yesod-http-signature.src;
                yesod_mail_send = deps.yesod-mail-send.src;
              } ''
                mkdir -p $out
                cp -r $vervis/* $out/
                mkdir -p $out/lib
                cp -r $darcs_lights $out/lib/darcs-lights
                cp -r $darcs_rev $out/lib/darcs-rev
                cp -r $dvara $out/lib/dvara
                cp -r $hit_graph $out/lib/hit-graph
                cp -r $hit_harder $out/lib/hit-harder
                cp -r $hit_network $out/lib/hit-network
                cp -r $http_client_signature $out/lib/http-client-signature
                cp -r $http_signature $out/lib/http-signature
                cp -r $persistent_email_address $out/lib/persistent-email-address
                cp -r $persistent_graph $out/lib/persistent-graph
                cp -r $persistent_migration $out/lib/persistent-migration
                cp -r $time_interval_aeson $out/lib/time-interval-aeson
                cp -r $ssh $out/lib/ssh
                cp -r $yesod_auth_account $out/lib/yesod-auth-account
                cp -r $yesod_http_signature $out/lib/yesod-http-signature
                cp -r $yesod_mail_send $out/lib/yesod-mail-send
              '';
              stack-sha256 = "nLMMb/tngIG6VU4kH7dPr9DCPTYYz9G9J9ko45l91x0=";
              sha256map = {
                "https://dev.angeley.es/s/fr33domlover/r/yesod-auth-account"."2d19eea0fae58897a02372a84cc48e7696a4e288" =
                  "/D7e+zMq/1bsHN3D8mNkoU/dZZIdiM4M7DC+GKUMKag=";
              };
              pkg-def-extras = [
                (hackage: { hsc2hs = hackage.hsc2hs."0.68.4".revisions.default; })
              ];
              #compiler-nix-name = "ghc883";
            };
          in  vervis;
         #final.symlinkJoin {
         #  name = "vervis";
         #  buildInputs = [];
         #  paths = builtins.attrValues vervisPkgs.vervis.components.exes;
         #  postBuild = ''
         #    true
         #  '';
         #};
        })
      ];
    };

    forAllSystems = f: lib.genAttrs allSystems (system: f {
      inherit system;
      pkgs = pkgsForSystem system;
    });

    inputMap = let
      tryGetValue = res: if res.success then res.value else null;
    in {
      n1 = lib.mapAttrsToList lib.nameValuePair inputs;

      n2 = let
        s0 = inputs // { self = {}; };
        s1 = lib.mapAttrs (k: v: v.inputs) (lib.filterAttrs (k: v: v ? inputs) s0);
        s2 = lib.mapAttrsRecursiveCond (s: !(s ? outPath)) (n: i: lib.nameValuePair (lib.concatStringsSep "." n) i) s1;
      in lib.concatMap lib.attrValues (lib.attrValues s2);

      n3 = lib.const [] (let # broken (but why?)
        s0 = inputs // { self = {}; };
        s1 = lib.mapAttrs (k: v: v.inputs) (lib.filterAttrs (k: v: v ? inputs) s0);
        s2 = lib.mapAttrsRecursiveCond (s: !(s ? outPath)) (n: i: lib.nameValuePair (lib.concatStringsSep "." n) i) s1;
        s3 = lib.concatMap lib.attrValues (lib.attrValues s2);
        s4 = lib.mapAttrs (k: v: v.inputs) (lib.filterAttrs (k: v: v ? inputs) (lib.listToAttrs s3));
        s5 = lib.mapAttrsRecursiveCond (s: !(s ? outPath)) (n: i: lib.nameValuePair (lib.concatStringsSep "." n) i) s4;
        s6 = lib.concatMap lib.attrValues (lib.attrValues s5);
      in tryGetValue (builtins.tryEval (lib.concatMap lib.attrValues (lib.attrValues s6))));
    };
  in {
    nixosConfigurations = let
      system = "x86_64-linux"; # So far it always is...
      pkgs = pkgsForSystem system;
      usr = {
        utils = import ./lib/utils.nix {
          inherit lib;
        };
        elisp = import ./lib/elisp.nix {
          inherit lib;
          pkgs = channels.lib.legacyPackages.${system};
        };
      };
      specialArgs = {
        inherit inputs usr;
        dag = let dagLib = import "${inputs.nixus}/dag.nix" lib lib;
        in dagLib.dag // { inherit (dagLib) types; };
        fetchPullRequest = fetchPullRequestForSystem system;

        domains = import ./secrets/domains.nix;
        hosts = import ./secrets/hosts.nix;
      };

      modulesFor = hostName: {
        inherit system specialArgs;
        modules = let
          # External modules
          inherit (inputs.home.nixosModules) home-manager;
          inherit (inputs.dwarffs.nixosModules) dwarffs;
          inherit (inputs.guix.nixosModules) guix;
          inherit (inputs.construct.nixosModules) matrix-construct;

          # Some common basic stuff
          core = ./profiles/core.nix;

          # The flake-ier common basic stuff
          global = {
            environment.etc."machine-id".text = builtins.hashString "md5" hostName;
            environment.pathsToLink = [ "/share/bios" ];
            networking = { inherit hostName; };

            nix.package = pkgs.nix;
            nix.registry = lib.mapAttrs (id: flake: {
              inherit flake;
              from = { inherit id; type = "indirect"; };
            }) (inputs // { nixpkgs = inputs.master; });
            nix.nixPath = [
              "nixpkgs=${channels.pkgs}"
              "nixos=${./configuration.nix}"
              "config=/etc/nixos/configuration.nix"
            ];

            system.configurationRevision = inputs.self.rev or "dirty";
            system.nixos.versionSuffix = let inherit (inputs) self;
              date = lib.substring 0 8 (self.lastModifiedDate or self.lastModified);
              rev = self.shortRev or "dirty";
            in lib.mkForce ".${date}.${rev}";

            system.extraSystemBuilderCmds = (''

              mkdir -p $out/flake/input

              # Link first-class inputs
              ${lib.concatMapStringsSep "\n" ({ name, value }: ''
                ln -s '${value}' "$out/flake/input/${name}"
              '') inputMap.n1}

              # Link second-class inputs
              ${(lib.concatMapStringsSep "\n" ({ name, value }: ''
                ln -s '${value}' "$out/flake/input/${name}"
              '') inputMap.n2)}

              # Link third-class inputs (skipped)
              ${lib.concatMapStringsSep "\n" ({ name, value }: ''
                ln -s '${value}' "$out/flake/input/${name}"
              '') inputMap.n3}

            '') + (if ! (inputs.self ? rev) then ''
              echo "Cannot complete a dirty configuration"
              exit 1
            '' else "");

            system.activationScripts.etcnixos = ''
              rm -f /etc/nixos && \
              ln -sfn /run/current-system/flake/input/self /etc/nixos || \
              true
            '';

            nixpkgs = {
              pkgs = pkgs // {
                iptables = pkgs.iptables-nftables-compat;
              };
            };
          };

          # Amend home-manager (inject modules, set common stuff)
          home = { config, ... }: {
            options.home-manager.users = lib.mkOption {
              type = with lib.types; attrsOf (submoduleWith {
                specialArgs = specialArgs // {
                  super = config;
                };
                modules = let
                  flakeModules = import ./modules/home-manager.nix;
                  baduk = {
                    imports = [ (import inputs.baduk) ];
                    baduk.sabaki.engines = lib.mkDefault [];
                  };
                  impermanence = import "${inputs.impermanence}/home-manager.nix";
                in flakeModules ++ [
                  baduk
                ];
              });
            };

            config = {
              home-manager.useGlobalPkgs = true;
            };
          };

          # Global options that hosts depend on
          local = { lib, ... }: {
            imports = [ "${toString ./hosts}/${hostName}" ];

            options = {
              headless = lib.mkOption {
                type = lib.types.bool;
                description = "Is a headless machine";
              };
            };
          };

          # Hack in the gnupg secrets module
          gnupg = import "${inputs.pr93659}/nixos/modules/security/gnupg.nix";

          # Plug in the impermanence module (not a flake :<)
          impermanence = import "${inputs.impermanence}/nixos.nix";

          # Set up any other pull request modules
          pulls = { config, ... }: let
            iwdModule = "services/networking/iwd.nix";
          in {
            disabledModules = [ iwdModule ];
            imports = [ (import "${inputs.pr75800}/nixos/modules/${iwdModule}" {
              inherit config pkgs;
              lib = import "${inputs.pr75800}/lib/default.nix";
            }) ];
          };

          # Import this flake's defined nixos modules
          flakeModules = import ./modules/nixos.nix;

        in flakeModules ++ [
          core global home local gnupg pulls
          home-manager dwarffs guix matrix-construct impermanence
        ];
      };
    in usr.utils.recImport {
      # Build a nixos system for each dir in ./hosts using modulesFor
      dir = ./hosts;
      _import = host: let
        modules = modulesFor host;
      in channels.modules.lib.nixosSystem modules // {
        inherit specialArgs modules; # This is extra spicy, but vaguely needed for nixus?
      };
    };

    # convenience...
    homeConfigurations = lib.genAttrs (builtins.attrNames inputs.self.nixosConfigurations)
      (host: inputs.self.nixosConfigurations.${host}.config.home-manager.users);

    legacyPackages = forAllSystems ({ pkgs, ... }: pkgs);

    overlay = import ./pkgs;

    overlays = listToAttrs (map (name: {
      name = lib.removeSuffix ".nix" name;
      value = import (./overlays + "/${name}");
    }) (attrNames (readDir ./overlays)));

    packages = forAllSystems ({ pkgs, ... }: lib.filterAttrs (_: p: (p.meta.broken or null) != true) {
      inherit (pkgs.emacsPackages) bitwarden ivy-exwm;
      inherit (pkgs.emacsPackages) flycheck-purescript eterm-256color;
      inherit (pkgs.emacsPackages) envrc emacsbridge font-lock-ext sln-mode;
      inherit (pkgs.emacsPackages) emacs-ffi explain-pause-mode;
      inherit (pkgs.dotnetPackages) azure-functions-core-tools;
      inherit (pkgs) dgit dejavu_nerdfont electronmail;
      inherit (pkgs) flarectl fsnoop git-pr-mirror greetd ipfscat;
      inherit (pkgs) matrix-appservice-irc mx-puppet-discord;
      inherit (pkgs.pleroma) pleroma_be pleroma_fe masto_fe;
      inherit (pkgs) nyxt pure sddm-chili shflags velox yacy;
    });

    defaultPackage = forAllSystems ({ pkgs, system, ... }:
      import inputs.nixus { deploySystem = system; } ({ config, lib, ... }: let
        inherit (config) nodes;
      in {
        defaults = { name, config, ... }: let
          nixos = inputs.self.nixosConfigurations.${name}.modules;
        in {
          host = "root@${nixos.specialArgs.hosts.wireguard.${name}}";

          nixpkgs = channels.modules;

          configuration = {
            _module.args = nixos.specialArgs;
            imports = nixos.modules;

            # Link raw hosts on each host (non-recursively)
            system.extraSystemBuilderCmds = ''

              mkdir -p $out/flake/hosts

              # Link other hosts (nonrecursively)
              ${lib.concatMapStringsSep "\n" ({ name, value }: ''
                ln -s '${value.config.system.build.toplevel}' "$out/flake/hosts/${name}"
              '') (lib.mapAttrsToList lib.nameValuePair inputs.self.nixosConfigurations)}

            '';
          };

          # Filter out "added to list of known hosts" spam from output
          deployScriptPhases.filter-known-hosts = lib.dag.entryBefore ["copy-closure"] ''
            exec 2> >(${pkgs.gnugrep}/bin/grep -v "list of known hosts")
          '';

          # Replace all usage of nix-copy-closure with `nix-copy`
          deployScriptPhases.nix-copy-alias = lib.dag.entryBefore ["copy-closure"] ''
            PATH=${lib.makeBinPath [ pkgs.nixUnstable ]}:$PATH
            alias nix-copy-closure="nix copy"
            export NIX_REMOTE=""
          '';

          # Git tag all systems and deployments
          deployScriptPhases.git-tag = let
            inherit (config.configuration) system;
          in lib.dag.entryAfter ["switch"] ''
            systempath=$(basename "${system.build.toplevel.outPath}")
            systemnum=${name}/system-$id
            echo Tagging $systempath >&2
            ${pkgs.git}/bin/git tag $systempath ${system.configurationRevision} || true
            echo Tagging $systemnum >&2
            ${pkgs.git}/bin/git tag $systemnum ${system.configurationRevision} || true
          '';

          successTimeout = lib.mkDefault 120;
          switchTimeout = lib.mkDefault 120;

          ignoreFailingSystemdUnits = true;
        };

        nodes = let
          hosts = builtins.attrNames (builtins.removeAttrs inputs.self.nixosConfigurations [
            "image"
          ]);
        in (lib.genAttrs hosts (_: {})) // {
          zeta.hasFastConnection = true;
          zeta.successTimeout = 240; # Zeta seems very slow...
          zeta.switchTimeout = 240; # maybe due to wireguard reloading?
        };
      })
    );

    apps = forAllSystems ({ pkgs, system, ... }: {
      nixos = {
        type = "app";
        program = pkgs.callPackage ./pkgs/lib/nixos.nix {} + "/bin/nixos";
      };
      nixus = {
        type = "app";
        program = inputs.self.defaultPackage.${system}.outPath;
      };
    });

    defaultApp = forAllSystems ({ system, ... }: inputs.self.apps.${system}.nixus);

    nixosModules = let
      mergeAll = lib.fold lib.recursiveUpdate {};
      pathsToAttrs = map (file:
        let cleanFile = lib.removeSuffix ".nix" (lib.removePrefix "./" (toString file));
        in lib.setAttrByPath (lib.splitString "/" cleanFile) (import file)
      );

      moduleList = (import ./modules/nixos.nix) ++ (import ./modules/home-manager.nix);
      profilesList = import ./profiles/list.nix;
    in mergeAll (pathsToAttrs moduleList) // { profiles = mergeAll (pathsToAttrs profilesList); };

    devShell = forAllSystems ({ system, ... }:
      let
        pkgs = import channels.pkgs { inherit system; };
        my = import ./pkgs pkgs pkgs;

        nixos = import ./pkgs/lib/nixos.nix { pkgs = pkgs // my; };
        flake-shell = import ./pkgs/lib/flake-shell.nix { inherit pkgs; };
      in pkgs.mkShell {
        nativeBuildInputs = with pkgs; let
          git-crypt = pkgs.git-crypt.overrideAttrs (attrs: rec {
            worktreePatch = fetchurl {
              name = "support-worktree-simple-version.patch";
              url = "https://github.com/AGWA/git-crypt/files/2771938/git-crypt-support-worktree-simple-version-patch.txt";
              sha256 = "1k477m6g3zjdarjr38lndh0kpgkp0yi8lg2iqdispfd4c85krrax";
            };
            patches = [ worktreePatch ];
          });
        in [
          git git-crypt git-secrets
          nixfmt flake-shell nixos
        ];

        shellHook = ''
          mkdir -p secrets
        '';

        NIX_CONF_DIR = with pkgs; let
          nixConf = ''
            ${lib.optionalString (builtins.pathExists /etc/nix/nix.conf)
              (builtins.readFile /etc/nix/nix.conf)}
            experimental-features = nix-command flakes ca-references
          '';
        in linkFarm "nix-conf-dir" ( [
          { name = "nix.conf"; path = writeText "flakes-nix.conf" nixConf; }
          { name = "registry.json"; path = /etc/nix/registry.json; }
          { name = "machines"; path = /etc/nix/machines; }
        ] );
      }
    );

    passthru = rec {
      inherit inputs channels config allSystems inputMap;

      #$ git config secrets.providers "nix eval --raw .#passthru.secrets"
      secrets = with lib.strings; concatMapStringsSep "\n" (replaceStrings [" "] ["\\s"]) ([
        (import ./secrets/git.github.nix).oauth-token
      ] ++ (attrNames (import ./secrets/wifi.networks.nix))
        ++ (map (n: n.psk) (attrValues (import ./secrets/wifi.networks.nix)))
        ++ (attrValues (import ./secrets/root.password.nix))
        ++ (attrValues (import ./secrets/leaf.password.nix))
        ++ (attrValues (import ./secrets/user.password.nix))
        ++ (attrValues (import ./secrets/user.description.nix))
        ++ (attrValues (import ./secrets/emacs.user.nix))
        ++ (attrValues (import ./secrets/git.user.nix))
        ++ (attrValues (import ./secrets/spotify.credentials.nix))
        ++ (attrValues (import ./secrets/steam.credentials.nix))
        ++ (attrValues (import ./secrets/weechat.credentials.nix))
        ++ (attrValues (import ./secrets/domains.nix))
        ++ (lib.flatten (map attrValues (attrValues (import ./secrets/hosts.nix))))
      );
    };
  };
}
