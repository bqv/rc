{ buildGoModule, withSources, lib }:

buildGoModule rec {
  name = "dendrite";
  version = src.shortRev;

  src = withSources.dendrite;

  vendorSha256 = "NnSxqjY5HcSnxWbn9OAperNhysMZbpaOVlR5EHfzPNA=";

  passthru.config = "${src}/dendrite-config.yaml";
}
