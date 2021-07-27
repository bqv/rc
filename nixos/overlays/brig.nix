inputs@{ brig, ... }: final: prev: {
  brig = if final.lib.versionAtLeast prev.brig.version "0.5.0"
  then builtins.trace "pkgs.brig: overlay expired" prev.brig
  else (with prev.brig; final.buildGoModule {
    name = "brig-${inputs.brig.lastModifiedDate}";
    inherit pname doCheck meta;
    version = inputs.brig.shortRev;
    src = inputs.brig;
    vendorSha256 = "9kVTRRQWJq3gMsIqrsJobOZTucF2beDiHwXOA1LaAOQ=";
  });
}
