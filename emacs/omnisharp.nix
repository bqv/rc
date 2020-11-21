{ config, lib, usr, pkgs, ... }:

let
  omnisharp-roslyn_1_37_4 = pkgs.omnisharp-roslyn.overrideAttrs (drv: rec {
    version = "1.37.4";
    src = pkgs.fetchurl {
      url = "https://github.com/OmniSharp/omnisharp-roslyn/releases/download/v${version}/omnisharp-mono.tar.gz";
      sha256 = "IgOfu2pnwOfeMJw6CWQ6BsljEaykKy/zsSQF1zy8dl4=";
      # date = 2020-11-21T05:13:45+0000;
    };
  });
  omnisharp-roslyn = if lib.versionAtLeast pkgs.omnisharp-roslyn.version "1.37.4"
  then builtins.trace "[emacs/omnisharp]: roslyn has advanced" pkgs.omnisharp-roslyn
  else omnisharp-roslyn_1_37_4;
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
