{ lib, callPackage, naersk, fetchgit, pam, ... }:

naersk.buildPackage rec {
  name = "greetd";
  version = "0.6.1";

  src = fetchgit {
    url = "https://git.sr.ht/~kennylevinsen/greetd";
    rev = version;
    sha256 = "Jeb9GkR2OQ5j3xYNU0N64xPb2jDXkkeTb+UlyRGPOYo=";
  };

  buildInputs = [
    pam
  ];

  passthru = {
    tuigreet = callPackage ./tuigreet.nix {};
  };
}
