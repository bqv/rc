{ config, pkgs, lib, ... }:

let
  cfg = config.isolation;

  unaryStringFunctionType = with lib.types; addCheck anything (f:
    lib.isFunction f && lib.isString (f (lib.functionArgs f))
  ) // { description = "unary function returning string"; };
in {
  options.isolation = with lib; {
    makeHostAddress = mkOption {
      type = unaryStringFunctionType;
      example = literalExample ''{ id, ... }: "10.''${id}".0.1'';
    };
    makeLocalAddress = mkOption {
      type = unaryStringFunctionType;
      example = literalExample ''{ id, ... }: "10.''${id}".0.2'';
    };
    scopes = mkOption {
      type = types.addCheck (types.listOf (types.submodule ({ config, ... }: {
        options = {
          id = mkOption {
            type = types.ints.u8;
          };
          name = mkOption {
            type = types.str;
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
          nixos = mkOption {
            type = types.listOf types.submodule (let
              inherit (config) name;
            in { config, ... }: {
              boot.isContainer = true;
              networking.hostName = mkDefault name;
              networking.useDHCP = false;
              assertions = [
                {
                  assertion =  config.privateNetwork -> lib.stringLength name < 12;
                  message = ''
                    Container name `${name}` is too long: When `privateNetwork` is enabled, container names can
                    not be longer than 11 characters, because the container's interface name is derived from it.
                    This might be fixed in the future. See https://github.com/NixOS/nixpkgs/issues/38509
                  '';
                }
              ];
            });
          };
          system = mkOption {
            type = types.package;
            default = config.nixos.config.system.build.toplevel;
            internal = true;
          };
        };
        config = {
          _module.args = { inherit (config) hostAddress localAddress; };
        };
      }))) (xs: let
        left = builtins.attrNames (builtins.listToAttrs (map (x: {
          inherit (x) name; value = null;
        }) xs));
        right = map (x: x.name) xs;
      in lib.length left == lib.length right);
      default = [];
    };
    machines = mkOption {
      type = types.attrsOf types.package;
      default = builtins.listToAttrs (map (value: {
        inherit (value) name;
        inherit value;
      }) cfg.scopes);
      internal = true;
    };
  };

  config = {
    containers = lib.mapAttrs (machine: {
      autoStart = true;
      enableTun = true;
      privateNetwork = true;
      inherit (machine) hostAddress localAddress;

      config.system.build.toplevel = machine.system;
    }) cfg.machines;
  };
}
