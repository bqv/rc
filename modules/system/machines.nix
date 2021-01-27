{ config, pkgs, lib, extraModules, ... }:

let
  cfg = config.isolation;

  specialArgs = config._module.args;

  unaryStringFunctionType = with lib.types; addCheck anything (f:
    lib.isFunction f && lib.isString (f (lib.functionArgs f))
  ) // { description = "unary function returning string"; };
in {
  options.isolation = with lib; {
    makeHostAddress = mkOption {
      type = types.nullOr unaryStringFunctionType;
      example = literalExample ''{ id, ... }: "10.''${id}.0.1"'';
      default = null;
    };
    makeHostAddress6 = mkOption {
      type = types.nullOr unaryStringFunctionType;
      example = literalExample ''{ id, ... }: "fc00:''${id}::1"'';
      default = null;
    };
    makeLocalAddress = mkOption {
      type = types.nullOr unaryStringFunctionType;
      example = literalExample ''{ id, ... }: "10.''${id}.0.2"'';
      default = null;
    };
    makeLocalAddress6 = mkOption {
      type = types.nullOr unaryStringFunctionType;
      example = literalExample ''{ id, ... }: "fc00:''${id}::0"'';
      default = null;
    };
    scopes = mkOption {
      type = types.attrsOf (types.submodule ({ config, name, ... }: {
        options = {
          id = mkOption {
            type = types.ints.u8;
          };
          name = mkOption {
            type = types.addCheck types.str (name: lib.stringLength name < 12);
            default = name;
          };
          hostAddress = mkOption {
            type = types.str;
            default = cfg.makeHostAddress {
              inherit (config) id name;
            };
          };
          localAddress = mkOption {
            type = types.str;
            default = cfg.makeLocalAddress {
              inherit (config) id name;
            };
          };
          baseModules = mkOption {
            type = types.listOf types.anything;
            default = import (pkgs.path + "/nixos/modules/module-list.nix");
          };
          extraModules = mkOption {
            type = types.listOf types.anything;
            default = extraModules;
          };
          nixos = mkOption {
            type = types.nullOr (types.submoduleWith {
              inherit specialArgs;
              modules = config.baseModules ++ config.extraModules ++ [(let
                inherit (config) name;
              in { config, ... }: {
                _module.args.pkgs = pkgs;
                boot.isContainer = true;
                networking.hostName = mkDefault name;
                networking.useDHCP = false;
                nixpkgs.system = pkgs.system;
                nixpkgs.pkgs = pkgs;
                nix.optimise.automatic = false;
                documentation.nixos.enable = lib.mkForce false;
                users.mutableUsers = lib.mkForce true;
              })];
            });
          };
          system = mkOption {
            type = types.package;
            default = config.nixos.system.build.toplevel;
            internal = true;
          };
        };
        config = {
          _module.args = { inherit (config) hostAddress localAddress; };
          nixos = lib.mkMerge cfg.common.nixos;
        };
      }));
      default = {};
    };
    common.nixos = mkOption {
      type = with types; coercedTo anything singleton (listOf anything);
      default = {...}: {};
    };
    machines = mkOption {
      type = types.listOf types.anything;
      default = map (scope: {
        inherit (scope) id name;
        value = { inherit (scope) hostAddress localAddress system; };
      }) (builtins.attrValues cfg.scopes);
      internal = true;
    };
  };

  config = {
    containers = lib.mapAttrs (name: machine: {
      autoStart = true;
      enableTun = true;
      privateNetwork = true;
      inherit (machine) hostAddress localAddress;

      path = machine.system;
    }) (builtins.listToAttrs cfg.machines);

    assertions = lib.concatMap (attr: lib.mapAttrsToList (id: machines: {
      assertion = lib.any (m: m.id == null) machines || lib.length machines == 1;
      message = ''Overlap of container.*.${attr} between: ${
        lib.concatMapStringsSep "" (m: "\n  + ${m.name} = ${id}") machines}'';
    }) (lib.groupBy (x: toString x.id) (lib.mapAttrsToList (name: value: {
      inherit name value;
      id = value.${attr};
    }) config.containers))) [
      "hostAddress" "hostAddress6"
      "localAddress" "localAddress6"
    ];
  };
}
