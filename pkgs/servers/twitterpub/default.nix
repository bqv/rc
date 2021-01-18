{ buildGoModule, withSources, lib }:

buildGoModule rec {
  pname = "twitterpub";
  version = src.shortRev;

  vendorSha256 = "XuQhbS9iLNrW6aSanPdJFJO07JbJ7/QPUlGiYo0WZoE=";

  src = withSources.twitterpub;
}
