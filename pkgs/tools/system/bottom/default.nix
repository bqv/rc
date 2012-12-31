{ naersk, flake }:

naersk.buildPackage rec {
  name = "bottom";
  version = flake.inputs.bottom.shortRev;

  src = flake.inputs.bottom;
}
