{
  description = "A highly structured configuration database.";

  epoch = 201909;

  inputs.master.url = "github:nixos/nixpkgs/master";
  inputs.staged.url = "github:nixos/nixpkgs/staging";
  inputs.small.url = "github:nixos/nixpkgs/nixos-unstable-small";
  inputs.large.url = "github:nixos/nixpkgs/nixos-unstable";

  inputs.nix.url = "github:nixos/nix/flakes";
  inputs.nix.inputs.nixpkgs.follows = "master";

  inputs.dwarffs.url = "github:edolstra/dwarffs";
  inputs.dwarffs.inputs.nixpkgs.follows = "master";

  inputs.home.url = "github:rycee/home-manager/bqv-flakes";
  inputs.home.inputs.nixpkgs.follows = "large";

  inputs.nur.url = "github:nix-community/NUR";
  inputs.nur.inputs.nixpkgs.follows = "small";

  inputs.naersk.url = "github:nmattia/naersk";
  inputs.naersk.inputs.nixpkgs.follows = "large";

  inputs.emacs = { url = "github:nix-community/emacs-overlay"; flake = false; };
  inputs.mozilla = { url = "github:mozilla/nixpkgs-mozilla"; flake = false; };
  inputs.snack = { url = "github:nmattia/snack"; flake = false; };
  inputs.napalm = { url = "github:nmattia/napalm"; flake = false; };
  inputs.bhipple = { url = "github:bhipple/nur-packages"; flake = false; };
  inputs.epkgs = { url = "github:bqv/nixpkgs/emacs-native-pkgs"; };

  outputs = inputs@{ self, master, staged, small, large,
    nix, dwarffs, home, nur, naersk,
    emacs, mozilla, snack, napalm, bhipple, epkgs
  }:
    let
      inherit (builtins) listToAttrs baseNameOf attrNames attrValues readDir trace concatStringsSep;
      inherit (master.lib) fold recursiveUpdate setAttrByPath mapAttrs genAttrs filterAttrs;
      inherit (master.lib) removeSuffix removePrefix splitString const;
      forAllSystems = genAttrs [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      diffTrace = left: right: string: value: if left != right then trace string value else value;

      config = { allowUnfree = true; };

      fetchPullRequestForSystem = system: args@{ id, rev ? null, sha256 ? master.lib.fakeSha256, ... }: 
        mapAttrs (k: v: trace "pkgs.${k} pinned to nixpks/pull/${toString id}" v)
          (import (builtins.fetchTarball {
            name = "nixpkgs-pull-request-${toString id}"; inherit sha256;
            url = if ! builtins.isNull rev
                  then "https://github.com/NixOS/nixpkgs/archive/${rev}.zip"
                  else "https://github.com/NixOS/nixpkgs/archive/pull/${toString id}/head.zip";
          }) {
            inherit system config;
            overlays = attrValues self.overlays;
          } // (removeAttrs args [ "id" "rev" "hash" ]));

      pkgsForSystem = system: import large rec {
        inherit system config;
        overlays = (attrValues self.overlays) ++ [
          (self: super: { master =
             mapAttrs (k: v: diffTrace (baseNameOf master) (baseNameOf super.path) "pkgs.${k} pinned to nixpkgs/master" v)
             (import master { inherit config system overlays; });
           })
          (self: super: { staged =
             mapAttrs (k: v: diffTrace (baseNameOf staged) (baseNameOf super.path) "pkgs.${k} pinned to nixpkgs/staging" v)
             (import staged { inherit config system overlays; });
           })
          (self: super: { small =
             mapAttrs (k: v: diffTrace (baseNameOf small) (baseNameOf super.path) "pkgs.${k} pinned to nixpkgs/nixos-unstable-small" v)
             (import small { inherit config system overlays; });
           })
          (self: super: { large =
             mapAttrs (k: v: diffTrace (baseNameOf large) (baseNameOf super.path) "pkgs.${k} pinned to nixpkgs/nixos-unstable" v)
             (import large { inherit config system overlays; });
           })
          (self: super: let
            pkgs = import epkgs { inherit config system; };
          in import emacs pkgs pkgs)
          #(import emacs)
          (import mozilla)
          (pkgs: const {
            naersk = naersk.lib.x86_64-linux;
            snack = pkgs.callPackage (import "${inputs.snack}/snack-lib");
            napalm = pkgs.callPackage inputs.napalm;
            inherit (import staged { inherit config system; }) libgccjit;
            inherit (import bhipple { inherit pkgs; }) gccemacs;
          })
          nix.overlay
          nur.overlay
          self.overlay
        ];
      };

    in {
      nixosConfigurations = import ./hosts rec {
        inherit inputs;
        system = "x86_64-linux";
        pkgs = pkgsForSystem system;
        nixpkgs = master;
        specialArgs = {
          usr = import ./lib/utils.nix { inherit (nixpkgs) lib; };
          nurModules = inputs.nur.nixosModules;
          nurOverlays = inputs.nur.overlays;
          naersk = inputs.naersk.lib;
          snack = pkgs.callPackage (import "${inputs.snack}/snack-lib");
          napalm = pkgs.callPackage inputs.napalm;

          domains = import ./secrets/domains.nix;
          fetchPullRequest = fetchPullRequestForSystem system;
        };
      };

      overlay = import ./pkgs;

      overlays = listToAttrs (map (name: {
        name = removeSuffix ".nix" name;
        value = import (./overlays + "/${name}");
      }) (attrNames (readDir ./overlays)));

      packages = forAllSystems (system: let
        pkgs = pkgsForSystem system;
      in filterAttrs (_: p: (p.meta.broken or null) != true) {
        inherit (pkgs.emacsPackages) bitwarden ivy-exwm flycheck-purescript eterm-256color;
        inherit (pkgs) dgit dejavu_nerdfont flarectl fsnoop;
        inherit (pkgs.guilePackages) guile-gcrypt guile-git guile-json guile-sqlite3;
        inherit (pkgs.guilePackages) guile-ssh guile-gnutls bytestructures;
        inherit (pkgs) guix matrix-appservice-irc matrix-construct mx-puppet-discord;
        inherit (pkgs.pleroma) pleroma_be pleroma_fe masto_fe;
        inherit (pkgs) pure sddm-chili shflags yacy;

        inherit (pkgs) nheko;
        inherit (pkgs.weechatScripts) weechat-matrix;
      });

      nixosModules = let
        mergeAll = fold recursiveUpdate {};
        pathsToAttrs = map (file:
          let
            cleanFile = removeSuffix ".nix" (removePrefix "./" (toString file));
          in setAttrByPath (splitString "/" cleanFile) (import file)
        );

        # modules
        moduleList = import ./modules/list.nix;
        modulesAttrs = mergeAll (pathsToAttrs moduleList);

        # profiles
        profilesList = import ./profiles/list.nix;
        profilesAttrs = { profiles = mergeAll (pathsToAttrs profilesList); };
      in modulesAttrs // profilesAttrs;

      secrets = concatStringsSep "\n" ([]
        ++ (attrValues (import ./secrets/domains.nix))
      );
    };
}
