{ buildGoModule, flake, lib }:

buildGoModule rec {
  pname = "twitterpub";
  version = flake.inputs.twitterpub.shortRev;

  vendorSha256 = "XuQhbS9iLNrW6aSanPdJFJO07JbJ7/QPUlGiYo0WZoE=";

  src = flake.inputs.twitterpub;
}
