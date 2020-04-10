{
  description = "A highly structured configuration database.";

  epoch = 201909;

  inputs.nixpkgs.url = "github:nixos/nixpkgs/master";
  inputs.small.url = "github:nixos/nixpkgs/nixos-unstable-small";
  inputs.large.url = "github:nixos/nixpkgs/nixos-unstable";

  inputs.dwarffs.url = "github:edolstra/dwarffs";
  inputs.home.url = "github:rycee/home-manager/bqv-flakes";
  inputs.nur.url = "github:nix-community/NUR";

  inputs.dwarffs.inputs.nixpkgs.follows = "nixpkgs";
  inputs.home.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nur.inputs.nixpkgs.follows = "nixpkgs";

  inputs.emacs = { url = "github:nix-community/emacs-overlay"; flake = false; };

  outputs = inputs@{ self, home, nixpkgs, small, large, dwarffs, nur, emacs }:
    let
      inherit (builtins) listToAttrs baseNameOf attrNames attrValues readDir;
      inherit (nixpkgs.lib) fold recursiveUpdate setAttrByPath;
      inherit (nixpkgs.lib) removeSuffix removePrefix splitString;
      system = "x86_64-linux";

      pkgs = import nixpkgs rec {
        inherit system;
        overlays = (attrValues self.overlays) ++ [
          (self: super: { small = import small { inherit config system; }; })
          (self: super: { large = import large { inherit config system; }; })
          (self: super: { pr = n: hash: import (super.fetchzip {
             name = "nixpkgs-pull-request-${toString n}";
             url = "https://github.com/NixOS/nixpkgs/archive/pull/${toString n}/head.zip";
             hash = if hash == null then nixpkgs.lib.fakeSri else hash;
           }) { inherit config system; }; })
          (import emacs)
        ];
        config = { allowUnfree = true; };
      };

    in {
      nixosConfigurations = import ./hosts (inputs // {
        inherit system pkgs;
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
