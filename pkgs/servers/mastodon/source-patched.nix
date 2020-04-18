{ runCommand, jq, pkgs }:

let
  src = import ./source-unpatched.nix { inherit pkgs; };
  version = import ./version.nix;

in
  runCommand "mastodon-src-patched" {
    buildInputs = [ jq ];
  } ''
    cp -r ${src} $out
    chmod -R u+w $out
    jq ". + {version: \"${version}\"}" ${src}/package.json > "$out/package.json"
  ''
