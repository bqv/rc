{ home, nixpkgs, nur, dwarffs, inputs, pkgs, system, ... }:

let
  inherit (nixpkgs) lib;

  utils = import ../lib/utils.nix { inherit lib; };

  inherit (utils) recImport;

  inherit (builtins) attrValues removeAttrs;

  config = hostName:
    lib.nixosSystem {
      inherit system;

      specialArgs.usr = { inherit utils; };
      specialArgs.nurModules = nur.nixosModules;
      specialArgs.nurOverlays = nur.overlays;

      modules = let
        inherit (home.nixosModules) home-manager;

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

          nixpkgs = { inherit pkgs; };
          nixpkgs.overlays = [
            (_: _: { configuration = inputs.self; })
            nur.overlay
          ];

          home-manager.useGlobalPkgs = true;
        };

        local = import "${toString ./.}/${hostName}";

        flakeModules = import ../modules/list.nix;

      in flakeModules ++ [ core global local home-manager
                           dwarffs.nixosModules.dwarffs ];

    };

  hosts = recImport {
    dir = ./.;
    _import = config;
  };
in hosts
