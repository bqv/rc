{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    (with dotnetCorePackages; combinePackages [
      sdk_2_1 sdk_3_0 sdk_3_1
    ])
    mono msbuild
    azure-cli
    dotnetPackages.azure-functions-core-tools
   #jetbrains.rider
  ];
}
