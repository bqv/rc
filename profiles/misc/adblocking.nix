{ pkgs, ... }:
let
  inherit (builtins) concatStringsSep;
  inherit (pkgs) fetchFromGitHub stdenv gnugrep;
  inherit (builtins) readFile fetchurl;

  hosts = stdenv.mkDerivation {
    name = "hosts";

    src = fetchFromGitHub {
      owner = "x0uid";
      repo = "SpotifyAdBlock";
      rev = "1776d1a2b8ff0b73229196f37e3164b9049cc090";
      hash = "sha256-6iWUSoxvcyM/+BLCIab5ixH2UIyzGo8CtLsFElvtU44=";
    };

    nativeBuildInputs = [ gnugrep ];

    installPhase = ''
      mkdir -p $out/etc

      # filter whitelist
      grep -Ev '(${whitelist})' hosts > $out/etc/hosts

      # filter blacklist
      cat << EOF >> $out/etc/hosts
      ${blacklist}
      EOF
    '';
  };

  whitelist = concatStringsSep "|" [ ".*pirate(bay|proxy).*" ];

  blacklist = concatStringsSep ''

    0.0.0.0 '' [
      "# auto-generated: must be first"

    ];

in { networking.extraHosts = readFile "${hosts}/etc/hosts"; }
