{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.fsharp-mode = {
    demand = true;
    hook = [
      { fsharp-mode-hook = "dotnet-mode"; }
    ];
  };
}
