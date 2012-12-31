args@{ config, options, lib, ... }:

let
  # Slightly flimsy but means I don't need two files for this.
  nixos = ! home-manager;
  home-manager = args ? nixosConfig;
in {
  options = builtins.foldl' (a: b: a // b) {} [
    (if home-manager then {

      home-manager.users = lib.mkOption {
        type = with lib.types; attrsOf (submoduleWith {
          inherit specialArgs;
          modules = [{
            options.home.packagesAttrs = lib.mkOption {
              type = lib.types.attrs;
              description = "Attrset based mirror of home.packages";
            };
            config.home.packagesAttrs = lib.mkOverride 100 (lib.zipAttrs
              (map (p: { ${if p ? pname then p.pname else p.name} = p; })
                config.home.packages));
          }];
        });
      };

    } else {})
    (if nixos then {

      environment.systemPackagesAttrs = lib.mkOption {
        type = lib.types.attrs;
        description = "Attrset based mirror of environment.systemPackages";
      };

    } else {})
  ];
  config = if nixos then {

    environment.systemPackagesAttrs = lib.mkOverride 100 (lib.zipAttrs
      (map (p: { ${if p ? pname then p.pname else p.name} = p; })
        config.environment.systemPackages));

  } else {};
}
