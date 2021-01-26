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
