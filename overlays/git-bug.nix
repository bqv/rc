inputs@{ git-bug, ... }: final: prev: {
  git-bug = if final.lib.versionAtLeast prev.git-bug.version "0.7.3"
  then builtins.trace "pkgs.git-bug: overlay expired" prev.git-bug
  else (with prev.git-bug; final.buildGoModule {
    name = "git-bug-${inputs.git-bug.lastModifiedDate}";
    inherit pname doCheck meta;
    version = inputs.git-bug.shortRev;
    src = inputs.git-bug;
    vendorSha256 = "0ex0rfdC/Pk2950a/nAd3V/1A8k624c6cJWe+12XDy8=";
  });
}
