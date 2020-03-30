inputs@{ home, nixpkgs, dwarffs, emacs, self, pkgs, system, ... }:
let
  inherit (nixpkgs) lib;

  utils = import ../lib/utils.nix { inherit lib; };

  inherit (utils) recImport;

  inherit (builtins) attrValues removeAttrs;

  config = hostName:
    lib.nixosSystem {
      inherit system;

      specialArgs.usr = { inherit utils; };
      specialArgs.nurModules = inputs.nur.nixosModules;
      specialArgs.nurOverlays = inputs.nur.overlays;

      modules = let
        inherit (home.nixosModules) home-manager;

        core = ../profiles/core.nix;

        global = {
          networking.hostName = hostName;
          nix.nixPath = [
            "nixpkgs=${nixpkgs}"
            "nixos-config=/etc/nixos/configuration.nix"
            "nixpkgs-overlays=/etc/nixos/overlays"
          ];

          system.configurationRevision = self.rev;

          nixpkgs = { inherit pkgs; };
          nixpkgs.overlays = [
            (_: _: { configuration = self; })
            inputs.nur.overlay
            (import emacs)
          ];
        };

        local = import "${toString ./.}/${hostName}.nix";

        # Everything in `./modules/list.nix`.
        flakeModules =
          attrValues (removeAttrs self.nixosModules [ "profiles" ]);

      in flakeModules ++ [ core global local home-manager
                           dwarffs.nixosModules.dwarffs ];

    };

  hosts = recImport {
    dir = ./.;
    _import = config;
  };
in hosts
