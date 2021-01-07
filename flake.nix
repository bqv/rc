#+title: NixOS System Configuration
#+author: bqv
#+email: nixos@fron.io
#+options: toc:nil num:nil

#+BEGIN_SRC nix
{
  description = "A highly structured configuration database.";

  inputs = {
    master.url = "github:nixos/nixpkgs/master";               #|.
    staged.url = "github:nixos/nixpkgs/staging";              #| |-- Nix
    small.url  = "github:nixos/nixpkgs/nixos-unstable-small"; #| |--   pkgs
    large.url  = "github:nixos/nixpkgs/nixos-unstable";       #|'

    rel2009.url = "github:nixos/nixpkgs/nixos-20.09";         #| Stable
    rel2003.url = "github:nixos/nixpkgs/nixos-20.03";         #| Old Stable
    rel1909 = { url = "github:nixos/nixpkgs/nixos-19.09"; flake = false; };
    rel1903 = { url = "github:nixos/nixpkgs/nixos-19.03"; flake = false; };
    rel1809 = { url = "github:nixos/nixpkgs/nixos-18.09"; flake = false; };
    rel1803 = { url = "github:nixos/nixpkgs/18.03"; flake = false; };
    pr75800.url = "github:nixos/nixpkgs/517b290754f6a7cc487ce11932a8b750f868324d"; #|\ Pull
    pr93659.url = "github:ju1m/nixpkgs/security.pass";                             #|/ Reqs
    pr99188.url = "github:atemu/nixpkgs/giara-init";                               #||
    pr96368.url = "github:islandusurper/nixpkgs/lbry-desktop";                     #||

    nix.url = "github:nixos/nix/progress-bar";                #|- Nix
    nix-ipfs.url = "github:obsidiansystems/nix/ipfs-develop"; #|  ^^^IPFS

    dwarffs.url = "github:edolstra/dwarffs";         #|- Dwarffs
    dwarffs.inputs.nix.follows = "/nix";             #|
    dwarffs.inputs.nixpkgs.follows = "/nix/nixpkgs"; #|

    home.url = "github:nix-community/home-manager"; #|- Home-manager
    home.inputs.nixpkgs.follows = "/master";        #|

    naersk.url = "github:nmattia/naersk";      #|- Naersk
    naersk.inputs.nixpkgs.follows = "/master"; #|

    hydra.url = "github:nixos/hydra";              #|- Hydra
    hydra.inputs.nix.follows = "/nix";             #|
    hydra.inputs.nixpkgs.follows = "/nix/nixpkgs"; #|

    guix.url = "github:emiller88/guix";      #|- Guix
    guix.inputs.nixpkgs.follows = "/master"; #|

    construct.url = "github:matrix-construct/construct"; #|- Construct
    construct.inputs.nixpkgs.follows = "/large";         #|

    apparmor.url = "github:bqv/apparmor-nix"; #|- Apparmor

    emacs.url = "github:nix-community/emacs-overlay"; # Emacs-overlay

    lisp.url = "github:nix-lisp/lisp-overlay"; # Lisp-overlay

    nyxt.url = "github:atlas-engineer/nyxt"; #|- Nyxt
    nyxt.inputs.nixpkgs.follows = "/master"; #|

    wayland.url = "github:colemickens/nixpkgs-wayland"; #|- Nixpkgs-wayland
    wayland.inputs.nixpkgs.follows = "/small";          #|

    agenix.url = "github:ryantm/agenix";          #|- AgeNix
    agenix.inputs.nixpkgs.follows = "/small";     #|
    agenix.inputs.flake-utils.follows = "/utils"; #|

    haskell.url = "github:input-output-hk/haskell.nix"; # Haskell.nix
    utils.url = "github:numtide/flake-utils";           # Flake-utils
    hardware.url = "github:nixos/nixos-hardware";       # Nixos-hardware

    xontribs.url = "github:bqv/xontribs"; #|- Xontribs
    xontribs.inputs = {
      nixpkgs.follows = "/master";
      prompt-bar = { url = "github:anki-code/xontrib-prompt-bar/68b3487e156ed3dce80578ebe552b6afa94c7eb8"; flake = false; };
      pipeliner = { url = "github:anki-code/xontrib-pipeliner/daccb6c8a67bbda799dfa2d6d8d829b5e9151c92"; flake = false; };
    };

    hnix-overlay = { url = "github:haskell-nix/hnix"; flake = false; };            # Hnix
    impermanence = { url = "github:nix-community/impermanence"; flake = false; };  # Impermanence
    mozilla = { url = "github:mozilla/nixpkgs-mozilla"; flake = false; };          # Nixpkgs-mozilla
    baduk = { url = "github:dustinlacewell/baduk.nix"; flake = false; };           # Baduk
    snack = { url = "github:nmattia/snack"; flake = false; };                      # Snack
    napalm = { url = "github:nmattia/napalm"; flake = false; };                    # Napalm
    statichask = { url = "github:nh2/static-haskell-nix"; flake = false; };        # Static Haskell
    anki-sync = { url = "github:ankicommunity/anki-sync-server/125f7bb1"; flake = false; }; # Anki Server
    conix = { url = "github:thenerd247/conix"; flake = false; };
    prompt-toolkit = { url = "github:bobhy/python-prompt-toolkit/th-threadsafe-load-2"; flake = false; };
    matrix-nio = { url = "github:poljar/matrix-nio/98f0c244"; flake = false; };
    weechat-matrix = { url = "github:poljar/weechat-matrix/d4158416"; flake = false; };
    sqlcmdline = { url = "github:sebasmonia/sqlcmdline"; flake = false; };
    fish-bass = { url = "github:edc/bass"; flake = false; };
    spotify-adblock = { url = "github:x0uid/spotifyadblock"; flake = false; };
    bottom = { url = "github:clementtsang/bottom/0.4.5"; flake = false; };
    twitterpub = { url = "github:bqv/twitterpub"; flake = false; };
    zsh-pure = { url = "github:sindresorhus/pure"; flake = false; };
    fsnoop = { url = "github:jeffwalter/fsnoop"; flake = false; };
    shflags = { url = "github:kward/shflags"; flake = false; };
    git-remote-ipfs = { url = "github:cryptix/git-remote-ipfs"; flake = false; };
    git-get = { url = "github:grdl/git-get"; flake = false; };
    git-pullrequest = { url = "github:google/git-pull-request-mirror"; flake = false; };
    dgit = { url = "github:quorumcontrol/dgit/v0.0.14-alpha"; flake = false; };
    wld = { url = "github:michaelforney/wld"; flake = false; };
    swc = { url = "github:bqv/swc"; flake = false; };
    velox = { url = "github:michaelforney/velox"; flake = false; };
    st-wl = { url = "github:michaelforney/st"; flake = false; };
    dmenu = { url = "github:michaelforney/dmenu"; flake = false; };
    emacs-bitwarden = { url = "github:seanfarley/emacs-bitwarden"; flake = false; };
    ivy-exwm = { url = "github:pjones/ivy-exwm"; flake = false; };
    flycheck-purescript = { url = "github:bsermons/flycheck-purescript"; flake = false; };
    eterm-256color = { url = "github:dieggsy/eterm-256color"; flake = false; };
    envrc = { url = "github:purcell/envrc"; flake = false; };
    emacsbridge = { url = "github:aardsoft/emacsbridge"; flake = false; };
    font-lock-ext = { url = "github:sensorflo/font-lock-ext"; flake = false; };
    sln-mode = { url = "github:sensorflo/sln-mode"; flake = false; };
    emacs-ffi = { url = "github:tromey/emacs-ffi"; flake = false; };
    explain-pause-mode = { url = "github:lastquestion/explain-pause-mode"; flake = false; };
    gnome-network-displays = { url = "git+https://gitlab.gnome.org/gnome/gnome-network-displays"; flake = false; };
    emacs-webkit = { url = "github:akirakyle/emacs-webkit"; flake = false; };
    giara = { url = "git+https://gitlab.gnome.org/world/giara"; flake = false; };
    ini2json = { url = "github:anubisss/ini2json"; flake = false; };
    mfs-replace-root = { url = "github:hsanjuan/mfs-replace-root"; flake = false; };
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
        {
          description = "nixos/anbox: use mainline drivers when available";
          id = 102341; hash = "zaQSv78mfV670eQZGmcTh6w4x02fGaSxMYQpiJjavr0=";
        }
      ];
      patches = map basePkgs.fetchpatch pullReqs;
      patchedTree = basePkgs.applyPatches {
        name = "nixpkgs-patched";
        src = basePkgs.path;
        inherit patches;
        postPatch = ''
          patch=$(printf '%s\n' ${builtins.concatStringsSep " " (map (p: p.outputHash or (builtins.baseNameOf p)) patches)} |
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
        (channelToOverlay { inherit system config; flake = "rel1909"; branch = "nixos-19.09"; })
        (channelToOverlay { inherit system config; flake = "rel1903"; branch = "nixos-19.03"; })
        (channelToOverlay { inherit system config; flake = "rel1809"; branch = "nixos-18.09"; })
        (channelToOverlay { inherit system config; flake = "rel1803"; branch = "nixos-18.03"; })
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
        })
        inputs.nix.overlay (final: prev: rec {
          nixFlakes = nix;
          nixUnstable = nix;
          nix = inputs.nix.packages.${system}.nix.overrideAttrs (drv: {
            patches = (drv.patches or []) ++ [
              (final.fetchpatch {
                name = "libfetcher-file.patch";
                url = "https://github.com/nixos/nix/pull/4153.diff";
                sha256 = "JfcswqOG0V5qlolxxYFOpqXJgENC4Adfk4J8r//tgfA=";
              })
            ];
            passthru = {
              inherit (inputs.nix.packages.${system}.nix) perl-bindings;
            };
          });
          inherit (inputs.nix.packages.${system}) nix-static;
          nix-ipfs = inputs.nix-ipfs.packages.${system}.nix;
          nix-ipfs-static = inputs.nix-ipfs.packages.${system}.nix-static;
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
        inputs.emacs.overlay
        inputs.lisp.overlay
        inputs.xontribs.overlay
        inputs.wayland.overlay
        inputs.agenix.overlay
        inputs.apparmor.overlay
        inputs.self.overlay
        (pkgs: lib.const {
          inherit ((import (patchNixpkgs channels.modules.legacyPackages.${system}) { inherit system; }).pkgs) azure-cli; # pending nixpkgs-pr 107663
          inherit (inputs.master.legacyPackages.${system}) plantuml-server; # missing
          inherit (inputs.small.legacyPackages.${system}) firefox firefox-unwrapped; # slow and broken
          inherit (inputs.large.legacyPackages.${system}) thunderbird obs-studio webkitgtk chromium qemu; # slow
        })
        (self: super: {
          androidenv.androidPkgs_9_0 = builtins.trace "pkgs.androidenv: neutered due to breakages" {
            androidsdk = self.hello;
            inherit (super.androidenv.androidPkgs_9_0) platform-tools;
            build-tools = [self.hello];
          };
        })
      ];
    };

    forAllSystems = f: lib.genAttrs allSystems (system: f {
      inherit system;
      pkgs = pkgsForSystem system;
      inherit (pkgs) lib;
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
    nixosConfigurations = builtins.mapAttrs (host: node: let
      system = "x86_64-linux"; # So far it always is...
      pkgs = channels.modules.legacyPackages.${system};
    in {
      config = node.configuration;
    }) inputs.self.defaultPackage.x86_64-linux.config.nodes;

    homeConfigurations = lib.genAttrs (builtins.attrNames inputs.self.nixosConfigurations)
      (host: inputs.self.nixosConfigurations.${host}.config.home-manager.users) //
    {
      epsilon = forAllSystems ({ pkgs, system, ... }:
        let
          inherit (pkgs) pkgsStatic nix nix-static termonad-with-packages;
          flakeModules = import ./modules/home-manager.nix;
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
              flake = inputs.self;
            };
            nixpkgs = {
              inherit config system;
            };
            home.packages = with pkgs; [ nixFlakes termonad ];
            imports = flakeModules ++ [ ./users/aion.nix ];
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
      value = import (./overlays + "/${name}") inputs;
    }) (builtins.filter (file: lib.hasSuffix ".nix" file) (attrNames (readDir ./overlays))));

    packages = forAllSystems ({ pkgs, ... }: lib.filterAttrs (_: p: (p.meta.broken or null) != true) {
      inherit (pkgs.emacsPackages) bitwarden ivy-exwm;
      inherit (pkgs.emacsPackages) flycheck-purescript eterm-256color;
      inherit (pkgs.emacsPackages) envrc emacsbridge font-lock-ext sln-mode;
      inherit (pkgs.emacsPackages) emacs-ffi explain-pause-mode;
      inherit (pkgs.dotnetPackages) azure-functions-core-tools;
      inherit (pkgs) dgit dejavu_nerdfont electronmail;
      inherit (pkgs) flarectl fsnoop git-pr-mirror greetd ini2json ipfscat;
      inherit (pkgs.pleroma) pleroma_be pleroma_fe masto_fe;
      inherit (pkgs) pure shflags twitterpub velox vervis yacy;
    });

    defaultPackage = forAllSystems ({ pkgs, system, ... }: let
      deployment = import ./deploy {
        nixpkgs = patchNixpkgs (channels.modules.legacyPackages.${system});
        deploySystem = system; # By habit, system is deployer, platform is target
      } ({ config, lib, ... }: let
        inherit (config) nodes;
      in {
        defaults = { name, config, ... }: let
          evalConfig = import "${patchNixpkgs pkgs}/nixos/lib/eval-config.nix";

          getPlatform = with lib.modules; { modules, specialArgs, ... }: let
            args = { config = null; options = null; inherit lib; } // specialArgs;
          in (mergeModules [] (collectModules "" modules args)).matchedOptions.platform.value;

          nixos = with inputs.self.nixosModules;
            let platform = (getPlatform hosts.${system}.${name});
            in hosts.${platform}.${name};

          vmsystem = { modules, pkgs, specialArgs, ... }: {
            system.build.vm = (evalConfig {
              inherit specialArgs;
              inherit (pkgs) system;
              modules = modules ++ [
                (import "${channels.modules}/nixos/modules/virtualisation/qemu-vm.nix")
              ];
            }).config.system.build.toplevel;
          };

          linkage = { config, pkgs, ... }: let
            systems = builtins.mapAttrs (host: _: with inputs.self;
              let platform = getPlatform nixosModules.hosts.${system}.${host};
              in defaultPackage.${platform}.config.nodes.${host}.configuration
            ) nodes;
          in {
            options.system.linkOtherSystems = lib.mkOption {
              type = lib.types.bool;
              default = true;
              description = "Whether to link other flake nodes to the system derivation.";
            };

            # Link raw hosts on each host (non-recursively)
            config.system = {
              extraSystemBuilderCmds = lib.mkIf config.system.linkOtherSystems (''
                mkdir -p $out/flake/hosts

                # Link other hosts (nonrecursively)
                ${lib.concatMapStringsSep "\n" ({ name, value }: ''
                  ln -s '${value.system.build.toplevel}' "$out/flake/hosts/${name}"
                '') (lib.mapAttrsToList lib.nameValuePair systems)}

                # Link host containers
                ${lib.concatMapStringsSep "\n" (host@{ name, value }: ''
                  mkdir -p $out/flake/container/${name}
                  ${lib.concatMapStringsSep "\n" (container@{ name, value }: ''
                    ln -s '${value.config.system.build.toplevel}' "$out/flake/container/${host.name}/${name}"
                  '') (lib.mapAttrsToList lib.nameValuePair value.containers)}
                '') (lib.mapAttrsToList lib.nameValuePair systems)}
              '');
            };
          };
        in {
          host = "root@${nixos.specialArgs.hosts.wireguard.${name}}";

          configuration = {
            imports = nixos.modules ++ [
             #linkage # TODO: figure out how to make this work
              vmsystem
            ];
            config = {
              secrets.baseDirectory = "/var/lib/secrets";
              _module.args = nixos.specialArgs;
            };
          };

          # Filter out "added to list of known hosts" spam from output
          deployScriptPhases.filter-known-hosts = lib.dag.entryBefore ["copy-closure"] ''
            # Remove known hosts spam
            pipeline -w { ${pkgs.gnugrep}/bin/grep --line-buffered -v "list of known hosts" }
            fdswap 1 2
            pipeline -w { ${pkgs.gnugrep}/bin/grep --line-buffered -v "list of known hosts" }
            fdswap 2 1
          '';

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

          privilegeEscalationCommand = []; # already root

          successTimeout = lib.mkDefault 120;
          switchTimeout = lib.mkDefault 120;

          ignoreFailingSystemdUnits = true;
          systemSwitcherDir = "/nix/node/";

          dirty = ! (inputs.self ? rev);
        };

        nodes = let
          hosts = builtins.attrNames inputs.self.nixosModules.hosts.${system};
        in (lib.genAttrs hosts (_: {})) // {
          delta.hasFastConnection = true; # it's local!
          image.enabled = false;
          zeta.panicAction = "false"; # we shouldn't reboot this carelessly
          zeta.hasFastConnection = true;
          zeta.successTimeout = 240; # Zeta seems very slow...
          zeta.switchTimeout = 240; # maybe due to wireguard reloading?
        };
      });
    in pkgs.runCommand "deployment" {
      outputs = [ "out" "systems" ] ++ builtins.attrNames (
        lib.filterAttrs (_: n: n.enabled) deployment.config.nodes
      );
      passthru = deployment;
    } ''
      mkdir -p $(dirname $out)
      ln -s ${deployment.config.deployScript} $out
      ${lib.concatStringsSep "" (lib.mapAttrsToList (host: node: if node.enabled then ''
        ln -s ${node.nodeDeployScript} ''$${host}
        mkdir -p $systems
        ln -s ${node.configuration.system.build.toplevel} $systems/${host}
      '' else "") deployment.config.nodes)}
    '');

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
      delta = rec {
        type = "app";
        inherit (inputs.self.nixosConfigurations.delta.config.system.build) toplevel;
        program = (pkgs.writeShellScript "test-delta" ''
          echo Deploying ${toplevel}
          exec doas ${toplevel}/bin/switch-to-configuration test $@
        '').outPath;
        bao = rec {
          type = "app";
          inherit (inputs.self.homeConfigurations.delta.bao.home) activationPackage;
          program = (pkgs.writeShellScript "test-delta-bao" ''
            echo Deploying ${activationPackage}
            exec ${activationPackage}/activate $@
          '').outPath;
        };
      };
    });

    defaultApp = forAllSystems ({ system, ... }: inputs.self.apps.${system}.delta);

    nixosModules = let
      mergeAll = lib.fold lib.recursiveUpdate {};
      pathsToAttrs = map (file:
        let cleanFile = lib.removeSuffix ".nix" (lib.removePrefix "${toString ./.}/" (toString file));
        in lib.setAttrByPath (lib.splitString "/" cleanFile) (import file)
      );
      nixFilesOf = builtins.filter (lib.hasSuffix ".nix");

      moduleList = (import ./modules/nixos.nix)
        ++ (import ./modules/home-manager.nix);

      profilesList = (lib.filesystem.listFilesRecursive ./profiles)
        ++ (builtins.filter (f: ! builtins.hasAttr (builtins.baseNameOf f) (builtins.readDir ./users))
                            (lib.filesystem.listFilesRecursive ./users));
    in (mergeAll (pathsToAttrs (nixFilesOf moduleList)))
    // (mergeAll (pathsToAttrs (nixFilesOf profilesList)))
    // {
      hosts = forAllSystems ({ pkgs, system, ... }: (let
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

        modulesFor = hostName: appendModules: let
          specialArgs = {
            inherit usr;
            flake = inputs.self;
            fetchPullRequest = fetchPullRequestForSystem system;

            domains = import ./secrets/domains.nix;
            hosts = import ./secrets/hosts.nix;

            modules = systemModules ++ [
              { _module.args = specialArgs; }
            ];
            extraModules = [];
          };

          # External modules
          inherit (inputs.home.nixosModules) home-manager;
          inherit (inputs.dwarffs.nixosModules) dwarffs;
          inherit (inputs.guix.nixosModules) guix;
          inherit (inputs.construct.nixosModules) matrix-construct;
          inherit (inputs.agenix.nixosModules) age;
          apparmor-nix = inputs.apparmor.nixosModule;

          # Some common basic stuff
          core = ./profiles/core.nix;

          # The flake-ier common basic stuff
          global = {
            environment.etc."machine-id".text = builtins.hashString "md5" hostName;
            environment.pathsToLink = [ "/share/bios" ];
            networking = { inherit hostName; };

            documentation.nixos.extraModuleSources = [./.]
              ++ lib.mapAttrsToList (_: x: x.outPath) inputs;

            nix.package = lib.mkDefault pkgs.nixFlakes;
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
              date = lib.substring 0 8 (self.lastModifiedDate or "19700101");
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

            config.home-manager = {
              useUserPackages = true;
              useGlobalPkgs = true;
              verbose = true;
            };
          };

          # Hack in the gnupg secrets module (fix docbook)
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

          flakeModules = import ./modules/nixos.nix;

          # Actual host config
          configuration = import "${toString ./hosts}/${hostName}";

          systemModules = flakeModules ++ [
            core global iwd gnupg
            dwarffs guix matrix-construct impermanence age apparmor-nix
          ];

          userModules = [
            home
            home-manager
          ];
        in {
          inherit system specialArgs;
          modules = systemModules ++ userModules ++ [
            configuration
          ] ++ appendModules;
        };

        forEachHost = do: usr.utils.recImport {
          # Build a nixos system for each dir in ./hosts using modulesFor
          dir = ./hosts;
          _import = do;
        };
      in forEachHost (host: let
        pkgs = channels.modules.legacyPackages.${system};
      in modulesFor host [])));
    };

    devShell = forAllSystems ({ system, ... }:
      let
        pkgs = import channels.pkgs { inherit system; };
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
          git git-crypt git-secrets nixfmt
        ];

        shellHook = ''
          mkdir -p secrets
        '';

        NIX_CONF_DIR = with pkgs; let
          nixConf = ''
            ${lib.optionalString (builtins.pathExists /etc/nix/nix.conf)
              (builtins.readFile /etc/nix/nix.conf)}
            experimental-features = nix-command flakes ca-references
            print-build-logs = true
            access-tokens = "github.com=${(import ./secrets/git.github.nix).oauth-token}"
          '';
        in linkFarm "nix-conf-dir" ( [
          { name = "nix.conf"; path = writeText "flakes-nix.conf" nixConf; }
          { name = "registry.json"; path = /etc/nix/registry.json; }
          { name = "machines"; path = /etc/nix/machines; }
        ] );
      }
    );

    lib = rec {
      inherit inputs channels config allSystems inputMap patchNixpkgs;
      patchedPkgs = patchNixpkgs (channels.modules.legacyPackages.x86_64-linux);

      #$ git config secrets.providers "nix eval --raw .#lib.secrets"
      secrets = import ./secrets { inherit lib; };
    };

    hydraJobs = rec {
      tarball = with inputs.self.legacyPackages.${builtins.currentSystem};
      { key ? toString ./secrets/keys/git }: runCommandLocal "nixrc" rec {
        src = builtins.storePath inputs.self.outPath;
        buildInputs = [ src git git-crypt ];
        outputs = [ "out" "tgz" ];
      } ''
        git clone --depth=1 file://$src $out && cd $out
        git-crypt unlock ${key}
        tar cvz $out > $tgz
      '';
      deployment = { system ? builtins.currentSystem }:
      (import "${tarball}/configuration.nix" {}).defaultPackage.${system};
    };
  };
}
#+END_SRC
