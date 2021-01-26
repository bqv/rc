{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.fsharp-mode = {
    enable = false;
    demand = true;
    hook = [
      { fsharp-mode-hook = "dotnet-mode"; }
    ];
    config = ''
      (defun project-try-vc (d) nil) ; Disable project.el because bugs
    '';
  };
}
