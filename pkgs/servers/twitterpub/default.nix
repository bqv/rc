{ buildGoModule, fetchFromGitHub, lib }:

buildGoModule rec {
  pname = "twitterpub";
  version = lib.substring 0 7 src.rev;

  vendorSha256 = "XuQhbS9iLNrW6aSanPdJFJO07JbJ7/QPUlGiYo0WZoE=";

  src = fetchFromGitHub {
    owner = "bqv";
    repo = "twitterpub";
    rev = "64259a566a8bc084381f1616332ccbdd3bff6166";
    sha256 = "016ya5071paihc4yar7qqf3cdkyi7a5910qrkmad0b61k4xmqkyn";
  };
}
