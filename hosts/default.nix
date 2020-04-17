{ inputs, nixpkgs, pkgs, system, ... }:

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
      specialArgs.naersk = inputs.naersk.lib;
      specialArgs.snack = pkgs.callPackage (import "${inputs.snack}/snack-lib");
      specialArgs.napalm = pkgs.callPackage inputs.napalm;

      modules = let
        inherit (inputs.home.nixosModules) home-manager;

        core = ../profiles/core.nix;

        global = {
          networking.hostName = hostName;

          nix.registry = lib.mapAttrs (id: flake: {
            inherit flake;
            from = { inherit id; type = "indirect"; };
          }) inputs;
          nix.nixPath = [
            "nixpkgs=${nixpkgs}"
            "nixos-config=/etc/nixos/configuration.nix"
            "nixpkgs-overlays=/etc/nixos/overlays"
          ];

          system.configurationRevision = inputs.self.rev
            or (throw "Cannot build from an unclean source tree!");

          system.extraSystemBuilderCmds = '' ln -s '${../.}' "$out/flake" '';

          nixpkgs = {
            inherit pkgs;
          };

          home-manager.useGlobalPkgs = true;
        };

        local = import "${toString ./.}/${hostName}";

        flakeModules = import ../modules/list.nix;

      in flakeModules ++ [ core global local home-manager
                           inputs.dwarffs.nixosModules.dwarffs ];
    };

  hosts = recImport {
    dir = ./.;
    _import = config;
  };
in hosts
