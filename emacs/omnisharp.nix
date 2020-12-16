{ config, lib, usr, pkgs, ... }:

let
  inherit (pkgs) omnisharp-roslyn;
in {
  emacs-loader.omnisharp = {
    demand = true;
    hook = [
      { csharp-mode-hook = "omnisharp-mode"; }
      { csharp-mode-hook = "company-mode"; }
      { csharp-mode-hook = "flycheck-mode"; }
    ];
    config = ''
      (setq omnisharp-server-executable-path "${omnisharp-roslyn}/bin/omnisharp")
      (eval-after-load 'company
        '(add-to-list 'company-backends 'company-omnisharp))
    '';
    systemDeps = [ omnisharp-roslyn ];
  };
}
