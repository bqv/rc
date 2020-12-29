{ config, options, lib, ... }:

let
  nixos = options ? environment;
  home-manager = options ? home;
in {
  options = builtins.foldl' (a: b: a // b) {} [
    (if home-manager then {
      home.packages' = lib.mkOption {
        type = lib.types.attrs;
      };
    } else {})
    (if nixos then {
      environment.systemPackages' = lib.mkOption {
        type = lib.types.attrs;
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
