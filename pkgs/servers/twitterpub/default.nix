{ buildGoModule, fetchFromGitHub, lib }:

buildGoModule rec {
  pname = "twitterpub";
  version = lib.substring 0 7 src.rev;

  vendorSha256 = "XuQhbS9iLNrW6aSanPdJFJO07JbJ7/QPUlGiYo0WZoE=";

  src = fetchFromGitHub {
    owner = "bqv";
    repo = "twitterpub";
    rev = "fb0a21efd27d2c80cdf807ca9439eb8f7a52df6b";
    sha256 = "06c508m4ylx5k3pfgwm688c8kwkydkwni7df4ncyx135clhwcl79";
  };
}
