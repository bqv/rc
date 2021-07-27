{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.swc-launch.server;
in

{
  imports = [
    ./velox.nix
  ];

  options = {
    services.swc-launch.server = {
     #name = mkOption {
     #  type = types.str;
     #  default = null;
     #  example = "velox";
     #  description = "libswc server to be launched";
     #};
      active_server = mkOption {
        type = types.str;
        internal = true;
        default = null;
        description = ''
          The libswc server to be used (i.e. the server that was enabled). Only
          one libswc server can be active at a time.
        '';
      };
      available_servers = mkOption {
        type = types.listOf types.str;
        internal = true;
        default = [];
        description = ''
          The list of libswc servers that have been enabled.
        '';
      };
    };
  };

  config = {
    # Determine the server to be used. Must only be one server active.
    services.swc-launch.server.active_server =
      let
        enabled_servers =
        map (s: "${s.name}") (
        filter (s: s.server.enable == true) (
          map (s: { name = "${s}"; server = cfg.${s}; }) cfg.available_servers
        ));
      in
      if length enabled_servers == 1 then
        head enabled_servers
      else if length enabled_servers > 1 then
        throw "Only one libswc server can be enabled at a time."
      else null;
  };
}
