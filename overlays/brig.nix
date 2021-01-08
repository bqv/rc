inputs@{ brig, ... }: final: prev: {
  brig = if final.lib.versionAtLeast prev.brig.version "0.5.0"
  then builtins.trace "pkgs.brig: overlay expired" prev.brig
  else (with prev.brig; final.buildGoModule {
    inherit pname version doCheck meta;
    src = final.fetchgit {
      url = src.meta.homepage;
      sha256 = "c9amv3xU+3P14NygJK2hTNR+lTdFK7eEj1eoqv2FTDs=";
    };
    vendorSha256 = "bajsbVMp5HnfUgG6pETK8Beeiax+BItVQh57sgmFbik=";
  });
}
