{ nix, nixos-rebuild, lib, substituteAll, path, jq }:

let
  makeProg = args: substituteAll (args // {
    dir = "bin";
    isExecutable = true;
  });

  fallback = import "${path}/nixos/modules/installer/tools/nix-fallback-paths.nix";

 #nixos-rebuild = makeProg {
 #  name = "nixos-rebuild";
 #  src = "${path}/nixos/modules/installer/tools/nixos-rebuild.sh";
 #  nix = nix.out;
 #  nix_x86_64_linux = fallback.x86_64-linux;
 #  nix_i686_linux = fallback.i686-linux;
 #  path = lib.makeBinPath [ jq ];
 #};
in nixos-rebuild.overrideAttrs (_: {
  postInstall = ''
    sed -i '/extraBuildFlags=()/s/()/("--log-format" "bar-with-logs")/' $target
  '';
})
