{ naersk, withSources }:

naersk.buildPackage rec {
  pname = "wireguard-vanity";
  src = withSources.wgvanity;
  version = src.shortRev;
}
