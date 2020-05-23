{ lib, fetchFromGitHub, python3Packages }:

let
  inherit (python3Packages) pyyaml;
  pexpect = let
    inherit (python3Packages) fetchPypi;
    pyyaml_nix = python3Packages.pexpect;
    pyyaml_480 = python3Packages.pexpect.overridePythonAttrs (o: rec {
      version = "4.8.0";
      src = fetchPypi {
        inherit version;
        inherit (o) pname;
        sha256 = "032cg337h8awydgypz6f4wx848lw8dyrj4zy988x0lyib4ws8rgw";
      };
    });
  in if lib.versionAtLeast pyyaml_nix.version "4.8.0" then pyyaml_nix else pyyaml_480;
in python3Packages.buildPythonApplication rec {
  pname = "xxh";
  src = fetchFromGitHub {
    owner = "xxh";
    repo = pname;
    rev = "2795f02078e3bad0969cb1b40bd4f324aaf05684";
    sha256 = "1py9f0va160ymwp2kd9qrhr4kxpwlfkam19xxgqgn7i3s70s4nyg";
  };
  version = src.rev;
  propagatedBuildInputs = [ pexpect pyyaml ];
}
