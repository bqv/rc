{ lib, mkYarnPackage, withSources }:

mkYarnPackage rec {
  src = withSources.cloudflare-cli;

  meta = let
    nodeinfo = builtins.fromJSON (builtins.readFile src.packageJSON);
  in {
    description = nodeinfo.description;
    homepage = nodeinfo.homepage;
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
}
