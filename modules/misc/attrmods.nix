args@{ config, options, lib, ... }:

let
  # Slightly flimsy but means I don't need two files for this.
  nixos = ! home-manager;
  home-manager = args ? nixosConfig;
in {
  options = builtins.foldl' (a: b: a // b) {} [
    (if home-manager then {

      home.packages' = lib.mkOption {
        type = lib.types.attrs;
        description = "Attrset based mirror of home.packages";
      };

    } else {})
    (if nixos then {

      environment.systemPackages' = lib.mkOption {
        type = lib.types.attrs;
        description = "Attrset based mirror of environment.systemPackages";
      };

    } else {})
  ];
  config = builtins.foldl' (a: b: a // b) {} [
    (if home-manager then {

      home.packages' = lib.mkOverride 100 (lib.zipAttrs
        (map (p: { ${if p ? pname then p.pname else p.name} = p; })
          config.home.packages));

    } else {})
    (if nixos then {

      environment.systemPackages' = lib.mkOverride 100 (lib.zipAttrs
        (map (p: { ${if p ? pname then p.pname else p.name} = p; })
          config.environment.systemPackages));

    } else {})
  ];
}
