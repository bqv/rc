{ lib, mkYarnPackage, withSources }:

let
  pkg = mkYarnPackage rec {
    src = withSources.cloudflare-cli;

    meta = {
      description = nodeinfo.description;
      homepage = nodeinfo.homepage;
      license = lib.licenses.mit;
      platforms = lib.platforms.all;
    };
  };
  nodeinfo = builtins.fromJSON (builtins.readFile pkg.packageJSON);
in pkg
