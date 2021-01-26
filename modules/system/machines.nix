{ config, pkgs, lib, ... }:

{
  options.isolation = with lib; {
    makeHostAddress = mkOption {
      type = types.function;
      exampleText = ''{ id, ... }: "10.''${id}".0.1'';
    };
    makeLocalAddress = mkOption {
      type = types.function;
      exampleText = ''{ id, ... }: "10.''${id}".0.2'';
    };
    scopes = mkOption {
      type = types.listOf types.submodule ({ config, ... }: {
        options = {
          id = mkOption {
            type = types.addCheck types.int (x: x>=0 && x<256);
          };
          name = mkOption {
            type = types.str;
          };
          system = mkOption {
            type = types.listOf types.submodule ({ config, ... }: {
                          boot.isContainer = true;
                          networking.hostName = mkDefault name;
                          networking.useDHCP = false;
                          assertions = [
                            {
                              assertion =  config.privateNetwork -> stringLength name < 12;
                              message = ''
                                Container name `${name}` is too long: When `privateNetwork` is enabled, container names can
                                not be longer than 11 characters, because the container's interface name is derived from it.
                                This might be fixed in the future. See https://github.com/NixOS/nixpkgs/issues/38509
                              '';
                            }
                          ];
            });
          };
        };
        config = {
          _module.args.hostAddress = cfg.makeHostAddress {
            inherit (config) id name;
          };
          _module.args.localAddress = cfg.makeLocalAddress {
            inherit (config) id name;
          };
        };
      });
    };
  };

  config = {
    nix.extraOptions = ''
      extra-platforms = ${toString config.nix.supportedPlatforms} i686-linux
    '';
  };
}
