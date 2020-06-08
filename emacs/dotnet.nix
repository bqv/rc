{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.dotnet = {
    demand = true;
    after = [ "csharp-mode" ];
    hook = [
      { csharp-mode = "dotnet-mode"; }
    ];
    systemDeps = with pkgs; [ dotnet-sdk ];
  };
}
