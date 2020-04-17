{
  description = "A highly structured configuration database.";

  epoch = 201909;

  inputs.master.url = "github:nixos/nixpkgs/master";
  inputs.staged.url = "github:nixos/nixpkgs/staging";
  inputs.small.url = "github:nixos/nixpkgs/nixos-unstable-small";
  inputs.large.url = "github:nixos/nixpkgs/nixos-unstable";

  inputs.dwarffs.url = "github:edolstra/dwarffs/83c13981993fa54c4cac230f2eec7241ab8fd0a9";
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

  outputs = inputs@{ self, master, staged, small, large,
    dwarffs, home, nur, naersk,
    emacs, mozilla, snack, napalm
  }:
    let
      inherit (builtins) listToAttrs baseNameOf attrNames attrValues readDir trace;
      inherit (master.lib) fold recursiveUpdate setAttrByPath mapAttrs genAttrs;
      inherit (master.lib) removeSuffix removePrefix splitString;
      forAllSystems = genAttrs [ "x86_64-linux" "x86_64-darwin" "i686-linux" "aarch64-linux" ];
      diffTrace = left: right: string: value: if left != right then trace string value else value;

      pkgsForSystem = system: import large rec {
        inherit system;
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
          (self: super: { pr = n: hash: 
             mapAttrs (k: v: trace "pkgs.${k} pinned to nixpks/pull/${toString n}" v)
             (import (super.fetchzip {
               name = "nixpkgs-pull-request-${toString n}";
               url = "https://github.com/NixOS/nixpkgs/archive/pull/${toString n}/head.zip";
               hash = if hash == null then master.lib.fakeSri else hash;
             }) { inherit config system overlays; });
           })
          (import emacs)
          (import mozilla)
          nur.overlay
        ];
        config = { allowUnfree = true; };
      };

    in {
      nixosConfigurations = import ./hosts rec {
        inherit inputs;
        system = "x86_64-linux";
        pkgs = pkgsForSystem system;
        nixpkgs = master;
      };

      overlay = import ./pkgs;

      overlays = listToAttrs (map (name: {
        name = removeSuffix ".nix" name;
        value = import (./overlays + "/${name}");
      }) (attrNames (readDir ./overlays)));

      packages = forAllSystems (system: let
        pkgs = pkgsForSystem system;
      in {
        inherit (pkgs) sddm-chili dgit dejavu_nerdfont matrix-construct pure;
        inherit (pkgs.emacsPackages) bitwarden ivy-exwm flycheck-purescript eterm-256color;
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
    };
}
