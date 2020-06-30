{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    (with dotnetCorePackages; combinePackages [
      sdk_2_1 sdk_3_0 sdk_3_1
    ])
    mono
    azure-cli
    dotnet2nix
  ];
}
