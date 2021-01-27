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
      example = literalExample ''{ id, ... }: "fc00:''${id}::2"'';
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
                boot.isContainer = true;
                networking.hostName = mkDefault name;
                networking.useDHCP = false;
                nixpkgs.system = pkgs.system;
                nixpkgs.pkgs = pkgs;
                nix.package = pkgs.nixFlakes;
                documentation.nixos.enable = false;
              })];
            });
            default = null;
          };
          system = mkOption {
            type = types.package;
            default = config.nixos.system.build.toplevel;
            internal = true;
          };
        };
        config = {
          _module.args = { inherit (config) hostAddress localAddress; };
        };
      }));
      default = {};
    };
    machines = mkOption {
      type = types.listOf types.anything;
      default = map (scope: {
        inherit (scope) name;
        value = { inherit (scope) hostAddress localAddress system; };
      }) (builtins.attrValues cfg.scopes);
      internal = true;
    };
  };

  config = {
    containers = lib.mapAttrs (machine: {
      autoStart = true;
      enableTun = true;
      privateNetwork = true;
      inherit (machine) hostAddress localAddress;

      path = machine.system;
    }) (builtins.listToAttrs cfg.machines);
  };
}
