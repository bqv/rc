args@{ config, options, lib, ... }:

let
  # Slightly flimsy but means I don't need two files for this.
  nixos = ! home-manager;
  home-manager = args ? nixosConfig;

  merge = xs: builtins.foldl' (a: b: a // b) {} xs;
in {
  options = merge [
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
  config = merge [
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
