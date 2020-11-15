{
  description = "A highly structured configuration database.";

  inputs = {
    master.url = "github:nixos/nixpkgs/master";               #|.
    staged.url = "github:nixos/nixpkgs/staging";              #| |-- Nix
    small.url  = "github:nixos/nixpkgs/nixos-unstable-small"; #| |--   pkgs
    large.url  = "github:nixos/nixpkgs/nixos-unstable";       #|'

    rel2009.url = "github:nixos/nixpkgs/nixos-20.09";         #| Stable
    rel2003.url = "github:nixos/nixpkgs/nixos-20.03";         #| Old Stable
    pr75800.url = "github:nixos/nixpkgs/517b290754f6a7cc487ce11932a8b750f868324d"; #|\ Pull
    pr93659.url = "github:ju1m/nixpkgs/security.pass";                             #|/ Reqs
    pr99188.url = "github:atemu/nixpkgs/giara-init";                               #||
    pr96368.url = "github:islandusurper/nixpkgs/lbry-desktop";                     #||

    nix.url = "github:nixos/nix";           #|- Nix
    nix.inputs.nixpkgs.follows = "/master"; #|

    dwarffs.url = "github:edolstra/dwarffs";    #|- Dwarffs
    dwarffs.inputs.nix.follows = "/nix";        #|
    dwarffs.inputs.nixpkgs.follows = "/master"; #|

    home.url = "github:nix-community/home-manager"; #|- Home-manager
    home.inputs.nixpkgs.follows = "/master";        #|

    naersk.url = "github:nmattia/naersk/58aa776"; #|- Naersk
    naersk.inputs.nixpkgs.follows = "/master";    #|  (cole-h fork)

    xontribs.url = "github:bqv/xontribs";        #|- Xontribs
    xontribs.inputs.nixpkgs.follows = "/master"; #|
    # BEGIN ignorethis
    xontribs.inputs.prompt-bar.follows = "/prompt-bar";
    prompt-bar.url = "github:anki-code/xontrib-prompt-bar/68b3487e156ed3dce80578ebe552b6afa94c7eb8";
    prompt-bar.flake = false;
    # TODO notthis
    xontribs.inputs.pipeliner.follows = "/pipeliner";
    pipeliner.url = "github:anki-code/xontrib-pipeliner/daccb6c8a67bbda799dfa2d6d8d829b5e9151c92";
    pipeliner.flake = false;
    # END ignorethis

    guix.url = "github:bqv/guix";            #|- Guix
    guix.inputs.nixpkgs.follows = "/master"; #|

    construct.url = "github:matrix-construct/construct"; #|- Construct
    construct.inputs.nixpkgs.follows = "/large";         #|

    nix-ipfs.url = "github:obsidiansystems/nix/ipfs-develop"; # NixIPFS

    emacs.url = "github:nix-community/emacs-overlay";           # Emacs-overlay
    nativecomp.url = "github:fejfighter/emacs/pgtk-nativecomp"; # Emacs-nativecomp
    nativecomp.flake = false;

    nyxt = { url = "github:atlas-engineer/nyxt"; flake = false; }; #|- Nyxt

    wayland.url = "github:colemickens/nixpkgs-wayland"; #|- Nixpkgs-wayland
    wayland.inputs.nixpkgs.follows = "/small";          #|

    haskell.url = "github:input-output-hk/haskell.nix"; # Haskell.nix
    utils.url = "github:numtide/flake-utils";           # Flake-utils
    hardware.url = "github:nixos/nixos-hardware";       # Nixos-hardware

    impermanence = { url = "github:nix-community/impermanence"; flake = false; }; # Impermanence
    mozilla = { url = "github:mozilla/nixpkgs-mozilla"; flake = false; };         # Nixpkgs-mozilla
    baduk = { url = "github:dustinlacewell/baduk.nix"; flake = false; };          # Baduk
    snack = { url = "github:nmattia/snack"; flake = false; };                     # Snack
    napalm = { url = "github:nmattia/napalm"; flake = false; };                   # Napalm
    statichask = { url = "github:nh2/static-haskell-nix"; flake = false; };       # Static Haskell
  };

  outputs = inputs: with builtins; let
    allSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
    diffTrace = left: right: string: value: if left != right then trace string value else value;

    # Nixos Config
    config = {
      allowUnfree = true;
      android_sdk.accept_license = true;
    };

    patchNixpkgs = basePkgs: let
      pullReqs = map (meta: {
        url = meta.url or "https://github.com/nixos/nixpkgs/pull/${toString meta.id}.diff";
        name = "nixpkgs-pull-request-${toString meta.id}";
        inherit meta;
        sha256 = meta.hash;
      }) [
       #{
       #  description = "nixos/iwd: add `networks` and `interfaces` option";
       #  id = 75800; hash = "ptMLTVfCwd3N3kWQwDMn7dIJ2DYtijSohGGep+OuT24=";
       #}
        {
          description = "apparmor: fix and improve the service";
          id = 101071; hash = "DRwN1+ubcWRuy6qb3jszJ88J+o5DM6WjR5akjEYC7Ck=";
        }
       #{
       #  description = "nixos/security.gnupg: provisioning GnuPG-protected secrets through the Nix store";
       #  id = 93659; hash = "3im5nSrlM32DQUeq0Yp1MHkUcQyLdCGbxfJjgcc9e78=";
       #}
      ];
      patches = map basePkgs.fetchpatch pullReqs;
      patchedTree = basePkgs.applyPatches {
        name = "nixpkgs-patched";
        src = basePkgs.path;
        inherit patches;
        postPatch = ''
          patch=$(printf '%s\n' ${builtins.concatStringsSep " " (map (p: p.outputHash) patches)} |
          sort | sha256sum | cut -c -7)
          echo "+patch-$patch" >.version-suffix
        '';
      };

      import_nixpkgs = args: import patchedTree ({ inherit (basePkgs) system; } // args);
    in patchedTree // {
      lib = (import_nixpkgs {}).lib;
      legacyPackages = basePkgs.lib.genAttrs allSystems (system: _: import_nixpkgs { inherit system; });
    };

    channels = with inputs; {
      pkgs = small;       # For packages
      modules = master;   # For nixos modules
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
        (channelToOverlay { inherit system config; flake = "staged"; branch = "staging"; })
        (channelToOverlay { inherit system config; flake = "small"; branch = "nixos-unstable-small"; })
        (channelToOverlay { inherit system config; flake = "large"; branch = "nixos-unstable"; })
        (channelToOverlay { inherit system config; flake = "rel2009"; branch = "nixos-20.09"; })
        (channelToOverlay { inherit system config; flake = "rel2003"; branch = "nixos-20.03"; })
        (final: prev: { broken = import channels.pkgs {
          inherit system overlays;
          config = config // { allowBroken = true; };
        }; })
        (final: prev: { insecuressl = import channels.pkgs {
          inherit system;
          config = config // { permittedInsecurePackages = [ "openssl-1.0.2u" ]; };
        }; })
        (flakeToOverlay { inherit system; flake = "lg400"; name = "delta/system-400-link"; })
        (import inputs.mozilla)
        (pkgs: raw: {
          inherit raw;
          naersk = inputs.naersk.lib.${system};
          snack = pkgs.callPackage (import "${inputs.snack}/snack-lib");
          napalm = pkgs.callPackage inputs.napalm;
          inherit (inputs.haskell.legacyPackages.${system}) haskell-nix; # ignore overlay, we want cache hits
          jamiPkgs = pkgs.callPackage inputs.jami;
          jami = pkgs.jamiPkgs.ring-client-gnome;
        })
        inputs.nix.overlay (final: prev: {
          nix-ipfs = inputs.nix-ipfs.defaultPackage.${system};
          nixFlakes = inputs.nix.packages.${system}.nix.overrideAttrs (drv: {
            patches = (drv.patches or []) ++ [
              (final.fetchpatch {
                name = "logformat-option.patch";
                url = "https://github.com/nixos/nix/pull/3961.diff";
                sha256 = "vHSkQ3SYk1rzde7aNHZpV8nOFB/dGYBU7NofBWwXyQk=";
              })
              (final.fetchpatch {
                name = "libfetcher-file.patch";
                url = "https://github.com/nixos/nix/pull/4153.diff";
                sha256 = "JfcswqOG0V5qlolxxYFOpqXJgENC4Adfk4J8r//tgfA=";
              })
            ];
          });
          inherit (inputs.nix.packages.${system}) nix-static;
        })
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
        inputs.emacs.overlay (final: prev: rec {
          gccEmacs = let
            version = inputs.nativecomp.shortRev;
            src = inputs.nativecomp;
          in final.emacsGcc.overrideAttrs (old: {
            name = "${old.name}-${version}";
            inherit src version;
           #prePatch = ''
           #  sed -i '/comp_deferred_compilation = true;/s/true/false/' src/comp.c
           #'';
            buildInputs = old.buildInputs ++ [ final.cairo ];
            configureFlags = old.configureFlags ++ [ "--with-pgtk" "--with-cairo" "--with-modules" ];
          });

          gccEmacsPackages = lib.dontRecurseIntoAttrs (final.emacsPackagesFor gccEmacs);

          # Overridden for exwm
          emacsWithPackages = gccEmacsPackages.emacsWithPackages;
        })
        inputs.xontribs.overlay
        inputs.wayland.overlay
        (final: prev: {
          nyxt = prev.nyxt.overrideAttrs (drv: rec {
            src = drv.src.overrideAttrs (drv: {
              src = inputs.nyxt;
              name = lib.replaceStrings [drv.meta.version] [version] drv.name;
            });
            version = inputs.nyxt.lastModifiedDate;
          });
        })
        inputs.self.overlay
        (pkgs: lib.const {
          inherit ((import (patchNixpkgs channels.modules.legacyPackages.${system}) { inherit system; }).pkgs)
            apparmor apparmor-utils apparmor-kernel-patches apparmorRulesFromClosure iputils inetutils;
          inherit (inputs.rel2009.legacyPackages.${system}) firefox thunderbird; # slow
          inherit (inputs.rel2009.legacyPackages.${system}) rust-analyzer; # broken
          graalvm8 = builtins.trace "pkgs.graalvm8: suspended - too big and not cached" pkgs.hello;
          lbry = (pkgs.symlinkJoin {
            name = "lbry";
            paths = [
              (pkgs.writeScriptBin "lbry-x11" ''
                #!${pkgs.runtimeShell}
                export DISPLAY=:0
                exec -a lbry ${inputs.pr96368.legacyPackages.${system}.lbry}/bin/lbry $@
              '')
              inputs.pr96368.legacyPackages.${system}.lbry
            ];
          }).overrideAttrs (_: { inherit (inputs.pr96368.legacyPackages.${system}.lbry) meta; });
          inherit (inputs.pr99188.legacyPackages.${system}) giara;
         #giara = (inputs.pr99188.legacyPackages.${system}.giara
         #).override { pkgs = pkgs // {
         #  inherit (pkgs.staged) webkitgtk libhandy;
         #  libical = pkgs.libical.overrideAttrs (drv: { doInstallCheck = false; });
         #}; inherit (pkgs) stdenv; };
         #giara-cairo = (inputs.pr99188.legacyPackages.${system}.giara.overrideAttrs (drv: {
         #  nativeBuildInputs = drv.nativeBuildInputs ++ [ pkgs.cairo ];
         #})).override { pkgs = pkgs // {
         #  inherit (pkgs.large) webkitgtk;
         #  inherit (pkgs.staged) libhandy;
         #  libical = pkgs.libical.overrideAttrs (drv: { doInstallCheck = false; });
         #}; inherit (pkgs) stdenv; };
          inherit (inputs.master.legacyPackages.${system}) nextcloud20; # doc eval problem
          postman = (pkgs.symlinkJoin {
            name = "postman";
            paths = [
              (pkgs.writeScriptBin "postman-x11" ''
                #!${pkgs.execline}/bin/execlineb -S0
                export DISPLAY :0
                exec -a postman
                ${pkgs.rel2003.postman}/bin/postman
              '')
              pkgs.rel2003.postman
            ];
          }).overrideAttrs (_: { inherit (pkgs.rel2003.postman) meta; });
          hnix = let
            inherit (inputs.staged.legacyPackages.${system}) haskell haskellPackages;
          in haskell.lib.overrideSrc haskellPackages.hnix {
            # PR: 554 derivationStruct
            src = pkgs.fetchFromGitHub {
              owner = "layus";
              repo = "hnix";
              rev = "derivationStrict";
              sha256 = "nUfvddtTKhmdauVvK8ErkjayiSXf78EVdK+Z/C+IDyA=";
            };
          };
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
        dag = let dagLib = import ./lib/dag.nix lib lib;
        in dagLib.dag // { inherit (dagLib) types; };
        units = {
          kilobytes = b: b * 1024;
          megabytes = k: k * 1024;
          gigabytes = m: m * 1024;
        };
      };
      specialArgs = {
        inherit usr;
        flakes = inputs;
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

            nix.package = pkgs.nixFlakes;
            nix.registry = lib.mapAttrs (id: flake: {
              inherit flake;
              from = { inherit id; type = "indirect"; };
            }) (inputs // { nixpkgs = inputs.master; });
            nix.nixPath = [
              "nixpkgs=${channels.pkgs}"
              "nixos=${inputs.self}/configuration.nix"
              "self=/run/current-system/flake/input/self/configuration.nix"
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

            '');

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
                inherit specialArgs;
                modules = let
                  flakeModules = import ./modules/home-manager.nix;
                  nixProfile = { lib, ... }: {
                    home.activation.disableNixEnv = lib.hm.dag.entryBefore ["installPackages"] ''
                      alias nix-env=true
                    '';
                    home.activation.installPackages = lib.mapAttrs (k: v: lib.mkForce v) (lib.hm.dag.entryAnywhere "true");
                  };
                  baduk = {
                    imports = [ (import inputs.baduk) ];
                    baduk.sabaki.engines = lib.mkDefault [];
                  };
                  impermanence = import "${inputs.impermanence}/home-manager.nix";
                in flakeModules ++ [
                  nixProfile
                  baduk
                ];
              });
            };

            config = {
              home-manager.useGlobalPkgs = true;
              home-manager.verbose = true;
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
          iwd = { config, ... }: let
            iwdModule = "services/networking/iwd.nix";
          in {
            disabledModules = [ iwdModule ];
            imports = [
              (import "${inputs.pr75800}/nixos/modules/${iwdModule}" {
                inherit config pkgs;
                lib = let
                  iwdLib = import "${inputs.pr75800}/lib/default.nix";
                in lib // {
                  types = {
                    inherit (iwdLib.types) fixedLengthString lengthCheckedString;
                  } // lib.types;
                };
              })
            ];
          };

          vm = import "${channels.modules}/nixos/modules/virtualisation/qemu-vm.nix";

          # Import this flake's defined nixos modules
          flakeModules = import ./modules/nixos.nix;

        in flakeModules ++ [
          core global home local gnupg iwd
          home-manager dwarffs guix matrix-construct impermanence
        ];
      };
    in usr.utils.recImport {
      # Build a nixos system for each dir in ./hosts using modulesFor
      dir = ./hosts;
      _import = host: let
        modules = modulesFor host;
        pkgs = channels.modules.legacyPackages.${system};
        nixosSystem = import "${patchNixpkgs pkgs}/nixos/lib/eval-config.nix" modules;
      in nixosSystem // {
        nixos = modules; # This is extra spicy, but vaguely needed for nixus?
      };
    };

    # convenience...
    homeConfigurations = lib.genAttrs (builtins.attrNames inputs.self.nixosConfigurations)
      (host: inputs.self.nixosConfigurations.${host}.config.home-manager.users) //
    {
      epsilon = forAllSystems ({ pkgs, system, ... }:
        let
          inherit (pkgs) pkgsStatic nix nix-static termonad-with-packages;
        in inputs.home.lib.homeManagerConfiguration rec {
          pkgs = pkgsStatic // {
            nixFlakes = nix;
            termonad = termonad-with-packages;
          };
          configuration = {
            _module.args = rec {
              pkgsPath = pkgs.path;
              inherit pkgs;
              nixosConfig = {
                services.aria2.rpcSecret = "";
                networking.hostName = "epsilon";
              };
              flakes = inputs;
            };
            nixpkgs = {
              inherit config system;
            };
            home.packages = with pkgs; [ nixFlakes termonad ];
            imports = [ ./users/aion.nix ];
          };
          inherit system;
          homeDirectory = "/home/${username}";
          username = "aion";
        }
      );
    };

    legacyPackages = forAllSystems ({ pkgs, ... }: pkgs);

    overlay = import ./pkgs;

    overlays = listToAttrs (map (name: {
      name = lib.removeSuffix ".nix" name;
      value = import (./overlays + "/${name}");
    }) (builtins.filter (file: lib.hasSuffix ".nix" file) (attrNames (readDir ./overlays))));

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
      inherit (pkgs) pure sddm-chili shflags twitterpub velox vervis yacy;
    });

    defaultPackage = forAllSystems ({ pkgs, system, ... }:
      import ./deploy rec {
        nixpkgs = patchNixpkgs (channels.modules.legacyPackages.${system});
        deploySystem = system;
      } ({ config, lib, ... }: let
        inherit (config) nodes;
      in {
        defaults = { name, config, ... }: let
          inherit (inputs.self.nixosConfigurations.${name}) nixos;
        in {
          host = "root@${nixos.specialArgs.hosts.wireguard.${name}}";

          configuration = {
            _module.args = nixos.specialArgs;
            imports = nixos.modules;

            secrets.baseDirectory = "/var/lib/secrets";

            # Link raw hosts on each host (non-recursively)
            system.extraSystemBuilderCmds = ''
              mkdir -p $out/flake/hosts

              # Link other hosts (nonrecursively)
              ${lib.concatMapStringsSep "\n" ({ name, value }: ''
                ln -s '${value.config.system.build.toplevel}' "$out/flake/hosts/${name}"
              '') (lib.mapAttrsToList lib.nameValuePair inputs.self.nixosConfigurations)}

              # Link host containers
              ${lib.concatMapStringsSep "\n" (host@{ name, value }: ''
                mkdir -p $out/flake/container/${name}
                ${lib.concatMapStringsSep "\n" (container@{ name, value }: ''
                  ln -s '${value.config.system.build.toplevel}' "$out/flake/container/${host.name}/${name}"
                '') (lib.mapAttrsToList lib.nameValuePair value.config.containers)}
              '') (lib.mapAttrsToList lib.nameValuePair inputs.self.nixosConfigurations)}
            '';
          };

          # Filter out "added to list of known hosts" spam from output
          deployScriptPhases.filter-known-hosts = lib.dag.entryBefore ["copy-closure"] ''
            # Remove known hosts spam
            pipeline -w { ${pkgs.gnugrep}/bin/grep --line-buffered -v "list of known hosts" }
            fdswap 1 2
            pipeline -w { ${pkgs.gnugrep}/bin/grep --line-buffered -v "list of known hosts" }
            fdswap 2 1
          '';

         ## Replace all usage of nix-copy-closure with `nix-copy`
         #deployScriptPhases.nix-copy-alias = lib.dag.entryBefore ["copy-closure"] ''
         #  alias nix-copy-closure="nix copy"
         #'';

          # Git tag all systems and deployments
          deployScriptPhases.git-tag = let
            inherit (config.configuration) system;
          in lib.dag.entryAfter ["trigger-switch"] ''
            foreground {
              backtick -i -n systemstorepath { basename "${system.build.toplevel.outPath}" }
              importas -i systemstorepath systemstorepath
              define systempath "nix/store/''${systemstorepath}"
              importas -i id ID
              define systemnum "${name}/system-''${id}"
              fdswap 1 2
              foreground { echo Tagging $systempath }
              foreground { ${pkgs.git}/bin/git tag $systempath "${system.configurationRevision}" }
              foreground { echo Tagging $systemnum }
              foreground { ${pkgs.git}/bin/git tag $systemnum "${system.configurationRevision}" }
              exit 0
            }
          '';

          successTimeout = lib.mkDefault 120;
          switchTimeout = lib.mkDefault 120;

          ignoreFailingSystemdUnits = true;
          systemSwitcherDir = "/nix/node/";

          dirty = ! (inputs.self ? rev);
        };

        nodes = let
          hosts = builtins.attrNames (builtins.removeAttrs inputs.self.nixosConfigurations [
            "image"
          ]);
        in (lib.genAttrs hosts (_: {})) // {
          zeta.panicAction = "false";
          zeta.hasFastConnection = true;
          zeta.successTimeout = 240; # Zeta seems very slow...
          zeta.switchTimeout = 240; # maybe due to wireguard reloading?
        };
      })
    );

    apps = forAllSystems ({ pkgs, system, ... }: {
      epsilon = rec {
        type = "app";
        inherit (inputs.self.homeConfigurations.epsilon.${system}) activationPackage;
        program = (pkgs.writeShellScript "reconfigure-epsilon" ''
          echo Deploying ${activationPackage}
          export HOST=epsilon
          export NIX_SSHOPTS="-o StrictHostKeyChecking=no"
          nix copy --to ssh://$HOST '${activationPackage}' \
            && ssh $NIX_SSHOPTS $HOST -t \
              sh -c ". $HOME/.nix-profile/etc/profile.d/nix.sh && \
                exec ${activationPackage}/activate $@"
        '').outPath;
      };
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
            log-format = bar-with-logs
          '';
        in linkFarm "nix-conf-dir" ( [
          { name = "nix.conf"; path = writeText "flakes-nix.conf" nixConf; }
          { name = "registry.json"; path = /etc/nix/registry.json; }
          { name = "machines"; path = /etc/nix/machines; }
        ] );
      }
    );

    passthru = rec {
      inherit inputs channels config allSystems inputMap patchNixpkgs;
      patchedPkgs = patchNixpkgs (channels.modules.legacyPackages.x86_64-linux);

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

      jamiDisk = let
        pkgs = inputs.master.legacyPackages.x86_64-linux;
        inherit (pkgs) fetchurl vmTools;

        debGen = {
          name = "jami";
          packages = vmTools.debDistros.debian9x86_64.packages ++ [
            "gnupg" "dirmngr" "ca-certificates" "curl" "sshd" "apt" "jami"
          ];
          inherit (vmTools.debDistros.debian9x86_64) urlPrefix;
          packagesLists = [ vmTools.debDistros.debian9x86_64.packagesList ] ++ [(fetchurl {
            url = "https://dl.jami.net/nightly/debian_9/dists/ring/main/binary-amd64/Packages.gz";
            sha256 = "XkG3gkktf5pGKOTYQFpmKGG4LQNHIsyWlmwk/Hz7Ocg=";
          })];
        };

        baseDebs = import (vmTools.debClosureGenerator debGen) { inherit fetchurl; };

        snapshot = builtins.head (lib.splitString "/dists/" (
          builtins.head vmTools.debDistros.debian9x86_64.packagesList.drvAttrs.urls)
        );
      in vmTools.fillDiskWithDebs rec {
        name = "jami";
        fullName = name + "-debian";
        debs = builtins.map (d: d.overrideAttrs (drv: { urls = let
          urlparts = lib.splitString "debian/" (builtins.head drv.urls);
        in [
          "${snapshot}/${lib.last urlparts}"
        ] ++ drv.urls ++ [
          "https://dl.jami.net/nightly/debian_9/${lib.last urlparts}"
        ]; })) (lib.flatten baseDebs);
      };
      jamiVM = let
        pkgs = inputs.master.legacyPackages.x86_64-linux;
        vm = pkgs.vmTools.runInLinuxVM (let
          nixosConfig = inputs.self.nixosConfigurations.delta.config;
          sshd_config = nixosConfig.environment.etc."ssh/sshd_config".source;
        in pkgs.runCommand "sshd" rec {
          diskImage = jamiDisk;
          passthru = { inherit diskImage; };
        } ''
          /usr/sbin/useradd --system sshd || true
          mkdir -p /etc/ssh /var/empty /var/run/sshd
          ${pkgs.openssh}/bin/ssh-keygen -A
          ${pkgs.openssh}/bin/sshd -f ${sshd_config}
        '');
      in vm; #pkgs.vmTools.makeImageTestScript jamiDisk;
    };
  };
}
