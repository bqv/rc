{
  description = "A highly structured configuration database.";

  epoch = 201909;

  inputs.master.url = "github:nixos/nixpkgs/master";
  inputs.staged.url = "github:nixos/nixpkgs/staging";
  inputs.small.url = "github:nixos/nixpkgs/nixos-unstable-small";
  inputs.large.url = "github:nixos/nixpkgs/nixos-unstable";

  inputs.dwarffs.url = "github:edolstra/dwarffs/83c13981993fa54c4cac230f2eec7241ab8fd0a9";
  inputs.home.url = "github:rycee/home-manager/bqv-flakes";
  inputs.nur.url = "github:nix-community/NUR";

  inputs.dwarffs.inputs.nixpkgs.follows = "master";
  inputs.home.inputs.nixpkgs.follows = "large";
  inputs.nur.inputs.nixpkgs.follows = "small";

  inputs.emacs = { url = "github:nix-community/emacs-overlay"; flake = false; };

  outputs = inputs@{ self, home, master, staged, small, large, dwarffs, nur, emacs }:
    let
      inherit (builtins) listToAttrs baseNameOf attrNames attrValues readDir;
      inherit (master.lib) fold recursiveUpdate setAttrByPath mapAttrs;
      inherit (master.lib) removeSuffix removePrefix splitString;
      system = "x86_64-linux";

      pkgs = import large rec {
        inherit system;
        overlays = (attrValues self.overlays) ++ [
          (self: super: { master =
             mapAttrs (k: v: builtins.trace "pkgs.${k} fetched from master" v)
             (import master { inherit config system overlays; });
           })
          (self: super: { staged =
             mapAttrs (k: v: builtins.trace "pkgs.${k} fetched from staging" v)
             (import staged { inherit config system overlays; });
           })
          (self: super: { small =
             mapAttrs (k: v: builtins.trace "pkgs.${k} fetched from nixos-unstable-small" v)
             (import small { inherit config system overlays; });
           })
          (self: super: { large =
             mapAttrs (k: v: builtins.trace "pkgs.${k} fetched from nixos-unstable" v)
             (import large { inherit config system overlays; });
           })
          (self: super: { pr = n: hash: import (super.fetchzip {
             name = "nixpkgs-pull-request-${toString n}";
             url = "https://github.com/NixOS/nixpkgs/archive/pull/${toString n}/head.zip";
             hash = if hash == null then master.lib.fakeSri else hash;
           }) { inherit config system overlays; }; })
          (import emacs)
        ];
        config = { allowUnfree = true; };
      };

    in {
      nixosConfigurations = import ./hosts (inputs // {
        inherit inputs system pkgs;
        nixpkgs = master;
      });

      overlay = import ./pkgs;

      overlays = listToAttrs (map (name: {
        name = removeSuffix ".nix" name;
        value = import (./overlays + "/${name}");
      }) (attrNames (readDir ./overlays)));

      packages.x86_64-linux = {
        inherit (pkgs) sddm-chili dgit dejavu_nerdfont matrix-construct pure;
        inherit (pkgs.emacsPackages) bitwarden ivy-exwm flycheck-purescript eterm-256color;
      };

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
