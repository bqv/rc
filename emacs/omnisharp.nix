{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.omnisharp = {
    demand = true;
    hook = [
      { csharp-mode-hook = "omnisharp-mode"; }
      { csharp-mode-hook = "company-mode"; }
      { csharp-mode-hook = "flycheck-mode"; }
    ];
    config = ''
      (setq omnisharp-server-executable-path "${pkgs.omnisharp-roslyn}/bin/omnisharp")
      (eval-after-load 'company
        '(add-to-list 'company-backends 'company-omnisharp))
    '';
    systemDeps = with pkgs; [ omnisharp-roslyn ];
  };
}
