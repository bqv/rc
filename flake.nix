{
  description = "A highly structured configuration database.";

  inputs = {
    master.url = "github:nixos/nixpkgs/master";
    staged.url = "github:nixos/nixpkgs/staging";
    small.url = "github:nixos/nixpkgs/nixos-unstable-small";
    large.url = "github:nixos/nixpkgs/nixos-unstable";
    pr75800.url = "github:ma27/nixpkgs/declarative-networks-with-iwd";
    traefik.url = "github:nixos/nixpkgs/f37b4d24d8872e82564c7b8f0ef5bc90476604d0";

    nix.url = "github:nixos/nix/flakes";
    nix.inputs.nixpkgs.follows = "master";

    dwarffs.url = "github:edolstra/dwarffs";
    dwarffs.inputs.nix.follows = "nix";
    dwarffs.inputs.nixpkgs.follows = "master";

    home.url = "github:rycee/home-manager/bqv-flakes";
    home.inputs.nixpkgs.follows = "large";

    naersk.url = "github:nmattia/naersk";
    naersk.inputs.nixpkgs.follows = "large";

    xontribs.url = "github:bqv/xontribs";
    xontribs.inputs.nixpkgs.follows = "large";

    guix.url = "github:bqv/guix";
    guix.inputs.nixpkgs.follows = "large";

    construct.url = "github:matrix-construct/construct";
    construct.inputs.nixpkgs.follows = "large";

    emacs.url = "github:nix-community/emacs-overlay";
    haskell.url = "github:input-output-hk/haskell.nix";
    nixus.url = "github:infinisil/nixus/545254808be876708535079996e2d9efd71f6533";
    utils.url = "github:numtide/flake-utils";

    mozilla = { url = "github:mozilla/nixpkgs-mozilla"; flake = false; };
    snack = { url = "github:nmattia/snack"; flake = false; };
    napalm = { url = "github:nmattia/napalm"; flake = false; };
    hardware = { url = "github:nixos/nixos-hardware"; flake = false; };
  };

  outputs = inputs: with builtins; let
    channels = with inputs; {
      pkgs = large;
      modules = master;
      lib = master;
    };
    inherit (channels.lib) lib;

    forAllSystems = lib.genAttrs [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
    diffTrace = left: right: string: value: if left != right then trace string value else value;

    # Nixos Config
    config = {
      allowUnfree = true;
      android_sdk.accept_license = true;
    };

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

    # Packages for nixos configs
    pkgsForSystem = system: import channels.pkgs rec {
      inherit system config;
      overlays = (attrValues inputs.self.overlays) ++ [
        (channelToOverlay { inherit system config; flake = "master"; branch = "master"; })
        (channelToOverlay { inherit system config; flake = "staged"; branch = "staging"; })
        (channelToOverlay { inherit system config; flake = "small"; branch = "nixos-unstable-small"; })
        (channelToOverlay { inherit system config; flake = "large"; branch = "nixos-unstable"; })
        (import inputs.mozilla)
        (pkgs: lib.const {
          inherit inputs;
          naersk = inputs.naersk.lib.${system};
          snack = pkgs.callPackage (import "${inputs.snack}/snack-lib");
          napalm = pkgs.callPackage inputs.napalm;
          qutebrowser = pkgs.small.qutebrowser;
        })
        inputs.nix.overlay
        inputs.guix.overlay
        inputs.construct.overlay
        inputs.emacs.overlay
        inputs.haskell.overlay
        inputs.xontribs.overlay
        inputs.self.overlay
      ];
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
              "nixpkgs-overlays=/etc/nixos/overlays"
            ];

            system.configurationRevision = inputs.self.rev or "dirty";
            system.nixos.versionSuffix = let inherit (inputs) self;
              date = lib.substring 0 8 (self.lastModifiedDate or self.lastModified);
              rev = self.shortRev or "dirty";
            in lib.mkForce ".${date}.${rev}";

            system.extraSystemBuilderCmds = ''
              ln -s '${./.}' "$out/flake"
            '' + (if ! (inputs.self ? rev) then ''
              echo "Cannot complete a dirty configuration"
              exit 1
            '' else "");

            nixpkgs = {
              inherit pkgs;
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

          local = import "${toString ./hosts}/${hostName}";

          flakeModules = import ./modules/nixos.nix;

        in flakeModules ++ [
          core global home local
          home-manager dwarffs guix matrix-construct
        ];
      };

      configFor = host: let
        modules = modulesFor host;
      in modules ++ [{
        options = {
          specialArgs = lib.mkOption {
            type = lib.types.attrs;
          };
          modules = lib.mkOption {
            type = lib.types.list;
          };
        };
        config = {
          inherit specialArgs modules;
        };
      }];
    in usr.utils.recImport {
      dir = ./hosts;
      _import = host: let
        modules = modulesFor host;
      in lib.nixosSystem modules // {
        inherit specialArgs modules;
      };
    };

    legacyPackages = forAllSystems pkgsForSystem;

    overlay = import ./pkgs;

    overlays = listToAttrs (map (name: {
      name = lib.removeSuffix ".nix" name;
      value = import (./overlays + "/${name}");
    }) (attrNames (readDir ./overlays)));

    packages = forAllSystems (system: let
      pkgs = pkgsForSystem system;
    in lib.filterAttrs (_: p: (p.meta.broken or null) != true) {
      inherit (pkgs.emacsPackages) bitwarden ivy-exwm;
      inherit (pkgs.emacsPackages) flycheck-purescript eterm-256color;
      inherit (pkgs.emacsPackages) envrc emacsbridge font-lock-ext sln-mode;
      inherit (pkgs.dotnetPackages) azure-functions-core-tools;
      inherit (pkgs) dgit dejavu_nerdfont electronmail;
      inherit (pkgs) flarectl fsnoop git-pr-mirror ipfscat;
      inherit (pkgs) matrix-appservice-irc mx-puppet-discord;
      inherit (pkgs.pleroma) pleroma_be pleroma_fe masto_fe;
      inherit (pkgs) next pure sddm-chili shflags velox yacy;
    });

    nixosModules = let
      mergeAll = lib.fold lib.recursiveUpdate {};
      pathsToAttrs = map (file:
        let cleanFile = lib.removeSuffix ".nix" (lib.removePrefix "./" (toString file));
        in lib.setAttrByPath (lib.splitString "/" cleanFile) (import file)
      );

      moduleList = (import ./modules/nixos.nix) ++ (import ./modules/home-manager.nix);
      modulesAttrs = mergeAll (pathsToAttrs moduleList);

      profilesList = import ./profiles/list.nix;
      profilesAttrs = { profiles = mergeAll (pathsToAttrs profilesList); };
    in modulesAttrs // profilesAttrs;

    devShell = forAllSystems (system:
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
        in [ git git-crypt git-secrets age rage nixfmt flake-shell nixos ];

        shellHook = ''
          mkdir -p secrets
        '';

        NIX_CONF_DIR = with pkgs; let
          nixConf = ''
            ${lib.optionalString (builtins.pathExists /etc/nix/nix.conf)
              (builtins.readFile /etc/nix/nix.conf)}
            experimental-features = nix-command flakes ca-references
          '';
        in linkFarm "nix-conf-dir" ( [ {
          name = "nix.conf";
          path = writeText "flakes-nix.conf" nixConf;
        } {
          name = "registry.json";
          path = /etc/nix/registry.json;
        } {
          name = "machines";
          path = /etc/nix/machines;
        } ] );
      }
    );

    passthru = {
      nixus = let
        inherit (import ./secrets/hosts.nix) wireguard;
      in inputs.nixus.lib.nixus ({ config, ... }: {
        options.nodes = lib.mkForce (lib.mapAttrs (host: nixos: lib.mkOption {
          type = with lib.types; attrsOf (submodule {
            options.configuration = submoduleWith {
              inherit (nixos) specialArgs modules;
            };
            options.host = lib.mkOption {
              type = str;
              default = "root@${wireguard.${name}}";
            };
          });
        }) inputs.self.nixosConfigurations);

        config.defaults = { name, ... }: {
          nixpkgs = channels.lib;
          configuration = { config, ... }: {
            networking.hostName = lib.mkDefault name;
          };
        };
      });

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
