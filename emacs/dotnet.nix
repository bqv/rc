{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.dotnet = {
    demand = true;
    after = [ "csharp-mode" ];
    hook = [
      { csharp-mode = "dotnet-mode"; }
    ];
    systemDeps = with pkgs.dotnetCorePackages; [
      (combinePackages [
        sdk_2_1 sdk_3_0 sdk_3_1
      ])
    ];
  };
}
