{ inputs, nixpkgs, pkgs, system, specialArgs, ... }:

let
  inherit (nixpkgs) lib;

  inherit (specialArgs.usr) recImport;

  inherit (builtins) attrValues removeAttrs;

  config = hostName:
    lib.nixosSystem {
      inherit system;

      inherit specialArgs;

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
            or (throw "Refusing to build from an unclean source tree!");

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
in recImport {
  dir = ./.;
  _import = config;
}
