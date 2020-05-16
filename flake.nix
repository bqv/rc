{
  description = "A highly structured configuration database.";

  inputs = {
    master.url = "github:nixos/nixpkgs/master";
    staged.url = "github:nixos/nixpkgs/staging";
    small.url = "github:nixos/nixpkgs/nixos-unstable-small";
    large.url = "github:nixos/nixpkgs/nixos-unstable";

    nix.url = "github:nixos/nix/flakes";
    nix.inputs.nixpkgs.follows = "master";

    dwarffs.url = "github:edolstra/dwarffs";
    dwarffs.inputs.nix.follows = "nix";
    dwarffs.inputs.nixpkgs.follows = "master";

    home.url = "github:rycee/home-manager/bqv-flakes";
    home.inputs.nixpkgs.follows = "large";

    nur.url = "github:nix-community/nur";
    nur.inputs.nixpkgs.follows = "small";

    naersk.url = "github:nmattia/naersk";
    naersk.inputs.nixpkgs.follows = "large";

    guix.url = "github:bqv/guix";
    construct.url = "github:bqv/charybdis";
    emacs.url = "github:nix-community/emacs-overlay";

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

    config = { allowUnfree = true; };

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

    channelToOverlay = { system, config, flake, branch }: (final: prev: { ${flake} =
      mapAttrs (k: v: diffTrace (baseNameOf inputs.${flake}) (baseNameOf prev.path) "pkgs.${k} pinned to nixpkgs/${branch}" v)
      (import inputs.${flake} { inherit config system; overlays = []; });
    });

    pkgsForSystem = system: import channels.pkgs rec {
      inherit system config;
      overlays = (attrValues inputs.self.overlays) ++ [
        (channelToOverlay { inherit system config; flake = "master"; branch = "master"; })
        (channelToOverlay { inherit system config; flake = "staged"; branch = "staging"; })
        (channelToOverlay { inherit system config; flake = "small"; branch = "nixos-unstable-small"; })
        (channelToOverlay { inherit system config; flake = "large"; branch = "nixos-unstable"; })
        (import inputs.mozilla)
        (pkgs: lib.const {
          naersk = inputs.naersk.lib.${system};
          snack = pkgs.callPackage (import "${inputs.snack}/snack-lib");
          napalm = pkgs.callPackage inputs.napalm;
          inherit (inputs.small.legacyPackages.${system}) pulseeffects;
          inherit (inputs.staged.legacyPackages.${system}) libgccjit sof-firmware;
        })
        inputs.nix.overlay
        inputs.guix.overlay
        inputs.construct.overlay
        inputs.emacs.overlay
        inputs.nur.overlay
        inputs.self.overlay
      ];
    };

  in {
    nixosConfigurations = let
      system = "x86_64-linux";
      pkgs = pkgsForSystem system;
      specialArgs = {
        inherit inputs;
        usr = import ./lib/utils.nix { inherit lib; };
        fetchPullRequest = fetchPullRequestForSystem system;

        nurModules = nur.nixosModules;
        nurOverlays = nur.overlays;

        domains = import ./secrets/domains.nix;
        hosts = import ./secrets/hosts.nix;
      };

      inherit (specialArgs) usr;
      config = hostName: lib.nixosSystem {
        inherit system;

        inherit specialArgs;

        modules = let
          inherit (inputs.home.nixosModules) home-manager;
          inherit (inputs.dwarffs.nixosModules) dwarffs;
          inherit (inputs.guix.nixosModules) guix;
          inherit (inputs.construct.nixosModules) matrix-construct;

          core = ./profiles/core.nix;

          global = {
            networking.hostName = hostName;

            nix.registry = lib.mapAttrs (id: flake: {
              inherit flake;
              from = { inherit id; type = "indirect"; };
            }) (inputs // { nixpkgs = inputs.master; });
            nix.nixPath = [
              "nixpkgs=${channels.pkgs}"
              "nixos-config=/etc/nixos/configuration.nix"
              "nixpkgs-overlays=/etc/nixos/overlays"
            ];

            system.configurationRevision = if (inputs.self ? rev) then inputs.self.rev else "dirty";

            system.extraSystemBuilderCmds = ''
              ln -s '${./.}' "$out/flake"
            '' + (if ! (inputs.self ? rev) then ''
              echo "Cannot switch to a dirty configuration"
              exit 1
            '' else "");

            nixpkgs = {
              inherit pkgs;
            };

            home-manager.useGlobalPkgs = true;
          };

          local = import "${toString ./hosts}/${hostName}";

          flakeModules = import ./modules/list.nix;

        in flakeModules ++ [
          core global local
          home-manager dwarffs guix matrix-construct
        ];
      };
    in usr.recImport {
      dir = ./hosts;
      _import = config;
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
      inherit (pkgs.emacsPackages) bitwarden ivy-exwm flycheck-purescript eterm-256color;
      inherit (pkgs) dgit dejavu_nerdfont electronmail flarectl fsnoop;
      inherit (pkgs) matrix-appservice-irc mx-puppet-discord;
      inherit (pkgs.pleroma) pleroma_be pleroma_fe masto_fe;
      inherit (pkgs) next pure sddm-chili shflags yacy;

      inherit (pkgs) nheko;
      inherit (pkgs.weechatScripts) weechat-matrix;
    });

    nixosModules = let
      mergeAll = lib.fold lib.recursiveUpdate {};
      pathsToAttrs = map (file:
        let cleanFile = lib.removeSuffix ".nix" (lib.removePrefix "./" (toString file));
        in lib.setAttrByPath (lib.splitString "/" cleanFile) (import file)
      );

      moduleList = import ./modules/list.nix;
      modulesAttrs = mergeAll (pathsToAttrs moduleList);

      profilesList = import ./profiles/list.nix;
      profilesAttrs = { profiles = mergeAll (pathsToAttrs profilesList); };
    in modulesAttrs // profilesAttrs;

    secrets = with lib.strings; concatMapStringsSep "\n" (replaceStrings [" "] ["\\s"]) ([
    ] ++ (attrNames (import ./secrets/wifi.networks.nix))
      ++ (map (n: n.psk) (attrValues (import ./secrets/wifi.networks.nix)))
      ++ (attrValues (import ./secrets/root.password.nix))
      ++ (attrValues (import ./secrets/user.password.nix))
      ++ (attrValues (import ./secrets/user.description.nix))
      ++ (attrValues (import ./secrets/git.user.nix))
      ++ (attrValues (import ./secrets/domains.nix))
      ++ (lib.flatten (map attrValues (attrValues (import ./secrets/hosts.nix))))
    );
  };
}
