{ lib, stdenv, fetchzip, writeShellScriptBin, dotnetCorePackages }:

let
  sdk = dotnetCorePackages.sdk_3_1;

  src = fetchzip {
    url = "https://github.com/Azure/azure-functions-core-tools/releases/download/3.0.2630/Azure.Functions.Cli.no-runtime.3.0.2630.zip";
    sha256 = "1qhc18yfk0hyhmgldzqig48ha3h5d59i9hakkfhk0lab5az0khy3";
    stripRoot = false;
  };

  wrapper = writeShellScriptBin "func" ''
    exec ${sdk}/bin/dotnet exec ${src}/func.dll $@
  '';
in wrapper.overrideAttrs (_: rec {
  pname = "azure-functions-core-tools";
  version = "3.0.2630";
  name = "${pname}-${version}";
})
