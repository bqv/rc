{
  description = "A highly structured configuration database.";

  inputs = rec {
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

    home.url = "github:rycee/home-manager/bqv-flakes"; #|- Home-manager
    home.inputs.nixpkgs.follows = "large";             #|

    naersk.url = "github:nmattia/naersk";    #|- Naersk
    naersk.inputs.nixpkgs.follows = "large"; #|

    xontribs.url = "github:bqv/xontribs";      #|- Xontribs
    xontribs.inputs.nixpkgs.follows = "large"; #|

    guix.url = "github:bqv/guix";          #|- Guix
    guix.inputs.nixpkgs.follows = "large"; #|

    construct.url = "github:matrix-construct/construct"; #|- Construct
    construct.inputs.nixpkgs.follows = "large";          #|

    wayland.url = "github:colemickens/nixpkgs-wayland"; #|- Nixpkgs-wayland
    wayland.inputs.nixpkgs.follows = "large";           #|

    sops.url = "github:mic92/sops-nix";    #|- Sops-nix
    sops.inputs.nixpkgs.follows = "large"; #|

    emacs.url = "github:nix-community/emacs-overlay";   # Emacs-overlay
    haskell.url = "github:input-output-hk/haskell.nix"; # Haskell.nix
    utils.url = "github:numtide/flake-utils";           # Flake-utils
    hardware.url = "github:nixos/nixos-hardware";       # Nixos-hardware

    mozilla = { url = "github:mozilla/nixpkgs-mozilla"; flake = false; }; # Nixpkgs-mozilla
    snack = { url = "github:nmattia/snack"; flake = false; };             # Snack
    napalm = { url = "github:nmattia/napalm"; flake = false; };           # Napalm

    nixexprs = { url = "https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz"; flake = false; };
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
        (flakeToOverlay { inherit system; flake = "lg531"; name = "delta/system-531-link"; })
        (flakeToOverlay { inherit system; flake = "lg400"; name = "delta/system-400-link"; })
        (import inputs.mozilla)
        (pkgs: lib.const {
          inherit inputs;
          naersk = inputs.naersk.lib.${system};
          snack = pkgs.callPackage (import "${inputs.snack}/snack-lib");
          napalm = pkgs.callPackage inputs.napalm;
        })
        inputs.nix.overlay
        inputs.guix.overlay
        inputs.construct.overlay
        inputs.emacs.overlay
        (final: prev: builtins.removeAttrs (inputs.haskell.overlay final prev) [ "harfbuzz" ])
        inputs.xontribs.overlay
        inputs.wayland.overlay
        inputs.self.overlay
        (pkgs: lib.const {
          inherit (inputs.stable.legacyPackages.${system}) firefox thunderbird webkitgtk mitmproxy;
          inherit (inputs.stable.legacyPackages.${system}) home-assistant;
          graalvm8 = builtins.trace "graalvm8: suspended - too big and not cached" pkgs.hello;
          nyxt = pkgs.lg531.nyxt.overrideAttrs (old: {
            buildInputs = old.buildInputs ++ (with pkgs.gst_all_1; [
              gstreamer gst-libav
              gst-plugins-base
              gst-plugins-good
              gst-plugins-bad
              gst-plugins-ugly
            ]);
          });
          inherit (pkgs.lg400) catt;
          inherit (inputs.pr93457.legacyPackages.${system}) apparmor apparmor-utils lvm2;
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
        fetchPullRequest = fetchPullRequestForSystem system;

        domains = import ./secrets/domains.nix;
        hosts = import ./secrets/hosts.nix;
      };

      modulesFor = hostName: {
        inherit system specialArgs;
        modules = let
          inherit (inputs.home.nixosModules) home-manager;
          inherit (inputs.dwarffs.nixosModules) dwarffs;
          inherit (inputs.sops.nixosModules) sops;
          inherit (inputs.guix.nixosModules) guix;
          inherit (inputs.construct.nixosModules) matrix-construct;

          core = ./profiles/core.nix;

          global = {
            networking = {
              inherit hostName;
            };

            nix.registry = lib.mapAttrs (id: flake: {
              inherit flake;
              from = { inherit id; type = "indirect"; };
            }) (inputs // { nixpkgs = inputs.master; });
            nix.nixPath = [
              "nixpkgs=${channels.pkgs}"
              "nixos-config=/etc/nixos/configuration.nix"
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
              rm -f /etc/nixos
              ln -sfn /run/current-system/flake/input/self /etc/nixos
            '';

            nixpkgs = {
              pkgs = pkgs // {
                iptables = pkgs.iptables-nftables-compat;
              };
            };
          };

          home = { config, ... }: {
            options.home-manager.users = lib.mkOption {
              type = with lib.types; attrsOf (submoduleWith {
                specialArgs = specialArgs // {
                  super = config;
                };
                modules = import ./modules/home-manager.nix;
              });
            };

            config = {
              home-manager.useGlobalPkgs = true;
            };
          };

          secrets = { ... }: {
            config = {
              sops = {
                gnupgHome = "/root/.gnupg";
                sshKeyPaths = [];
              };
            };
          };

          local = { lib, ... }: {
            imports = [ "${toString ./hosts}/${hostName}" ];

            options = {
              headless = lib.mkOption {
                type = lib.types.bool;
                description = "Is a headless machine";
              };
            };
          };

          apparmor = { ... }: {
            environment.etc."ld-nix.so.preload".text = lib.mkDefault "";
          };

          gnupg = import "${inputs.pr93659}/nixos/modules/security/gnupg.nix";

          flakeModules = import ./modules/nixos.nix;

        in flakeModules ++ [
          core global home secrets local apparmor gnupg
          home-manager dwarffs sops guix matrix-construct
        ];
      };
    in usr.utils.recImport {
      dir = ./hosts;
      _import = host: let
        modules = modulesFor host;
      in channels.modules.lib.nixosSystem modules // {
        inherit specialArgs modules;
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

    defaultPackage = forAllSystems ({ pkgs, ... }: pkgs.linkFarm "nixrc" (
      (lib.mapAttrsToList (host: { config, ... }:
        { name = "nixosConfigurations/${host}"; path = config.system.build.toplevel; }
      ) inputs.self.nixosConfigurations)
    ));

    apps = forAllSystems ({ pkgs, ... }: {
      nixos = {
        type = "app";
        program = pkgs.callPackage ./pkgs/lib/nixos.nix {} + "/bin/nixos";
      };
    });

    defaultApp = forAllSystems ({ system, ... }: inputs.self.apps.${system}.nixos);

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
        ] ++ builtins.filter lib.isDerivation (builtins.attrValues inputs.sops.packages.${system});

        sopsPGPKeyDirs = [
          "./secrets/hosts"
          "./secrets/users"
        ]; # imports all files ending in .asc/.gpg and sets $SOPS_PGP_FP.
        sopsPGPKeys = [
         #"./secrets/users/bao.asc"
         #"./secrets/hosts/delta.asc"
        ]; # Also single files can be imported.

        shellHook = ''
          mkdir -p secrets/{hosts,users}
          sopsPGPHook
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
