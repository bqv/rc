{ naersk, flake }:

naersk.buildPackage rec {
  pname = "wireguard-vanity";
  src = flake.inputs.wgvanity;
  version = src.shortRev;
}
