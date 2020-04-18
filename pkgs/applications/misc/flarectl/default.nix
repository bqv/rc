{ stdenv, pkgs, fetchurl, nodejs, jq }:

let
  nodePackages = import ./flarectl.nix {
    inherit pkgs;
    inherit (stdenv.hostPlatform) system;
    inherit nodejs;
  };
in stdenv.mkDerivation rec {
  version = "1.0.0";
  name = "flarectl-${version}";

  src = fetchurl {
    url = "https://registry.npmjs.org/flarectl/-/${name}.tgz";
    sha256 = "0jkxi3b99qna6552bhdv26bi51jkxwr3jfp6q0m90kcblf68r9xc";
  };

  # node is the interpreter used to run this script
  buildInputs = [ jq nodejs ];

  buildPhase = ''
    ln -s ${nodePackages.shell.nodeDependencies}/lib/node_modules
  '';

  installPhase = ''
    cp -r . $out
    mv $out/bin/flarectl.js $out/bin/flarectl
    mv $out/bin/flarectl-ip.js $out/bin/flarectl-ip
    mv $out/bin/flarectl-zones.js $out/bin/flarectl-zones
  '';

  meta = with stdenv.lib; {
    description = "CloudFlare CLI";
    longDescription = ''
      A CLI control for your CloudFlare account, 
      powered by the cloudflare module. '';
    homepage = https://nodejs.org;
    license = licenses.bsd2;
    platforms = platforms.linux;
  };
}
