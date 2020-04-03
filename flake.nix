{
  description = "A highly structured configuration database.";

  epoch = 201909;

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable-small";
  inputs.guixpkgs.url = "github:bqv/nixpkgs/guix";
  inputs.dwarffs.url = "github:edolstra/dwarffs/47218f1b8f971925241b1b307a1e770a7c220b5e";
  inputs.home.url = "github:rycee/home-manager/bqv-flakes";
  inputs.nur.url = "github:nix-community/NUR";

  inputs.emacs = {
    type = "github";
    owner = "nix-community";
    repo = "emacs-overlay";
    flake = false;
  };
  inputs.vmnix = {
    type = "github";
    owner = "nekroze";
    repo = "vms.nix";
    flake = false;
  };

  outputs = inputs@{ self, home, nixpkgs, dwarffs, nur, emacs, vmnix, guixpkgs }:
    let
      inherit (builtins) listToAttrs baseNameOf attrNames readDir;
      inherit (nixpkgs.lib) removeSuffix;
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        overlays = self.overlays ++
          [(self: super: { inherit (import guixpkgs { inherit system; }) guix; })];
        config = { allowUnfree = true; };
      };

    in {
      nixosConfigurations =
        let configs = import ./hosts (inputs // { inherit system pkgs; });
        in configs;

      overlay = import ./pkgs;

      overlays = let
        overlays = map (name: import (./overlays + "/${name}"))
          (attrNames (readDir ./overlays));
      in overlays;

      packages.x86_64-linux = {
        inherit (pkgs) sddm-chili dejavu_nerdfont purs pure;
      };

      nixosModules = let
        prep = map (path: {
          name = removeSuffix ".nix" (baseNameOf path);
          value = import path;
        });

        # modules
        moduleList = import ./modules/list.nix;
        modulesAttrs = listToAttrs (prep moduleList);

        # profiles
        profilesList = import ./profiles/list.nix;
        profilesAttrs = { profiles = listToAttrs (prep profilesList); };

      in modulesAttrs // profilesAttrs;
    };
}
