{ buildGoModule, withSources, lib }:

buildGoModule rec {
  name = "dendrite";
  version = src.shortRev;

  src = withSources.dendrite;

  vendorSha256 = "CDCgp693pM+83ATPzmE35utYvQPb5sFal0xN5oasKSg=";

  passthru.config = "${src}/dendrite-config.yaml";
}
