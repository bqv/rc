{ pkgs, flake, ... }:
let
  inherit (builtins) concatStringsSep;
  inherit (pkgs) stdenv gnugrep;
  inherit (builtins) readFile fetchurl;

  hosts = stdenv.mkDerivation {
    name = "hosts";

    src = flake.inputs.spotify-adblock;

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
