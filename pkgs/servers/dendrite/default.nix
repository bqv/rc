{ buildGoModule, withSources, lib }:

buildGoModule rec {
  name = "dendrite";
  version = src.shortRev;

  src = withSources.dendrite;

  subPackages = [
    "cmd/dendrite-monolith-server"
    "cmd/generate-keys"
    "cmd/generate-config"
    "cmd/create-account"
    "cmd/create-room-events"
  ];
  vendorSha256 = "NnSxqjY5HcSnxWbn9OAperNhysMZbpaOVlR5EHfzPNA=";
}
