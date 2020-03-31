{ pkgs, ... }: {
  imports = [ ./zsh ./tmux ];

  environment.shellAliases = { v = "$EDITOR"; };

  environment.sessionVariables = {
    PAGER = "less";
    LESS = "-iFJMRWX -z-4 -x4";
    LESSOPEN = "|${pkgs.lesspipe}/bin/lesspipe.sh %s";
    EDITOR = "vim";
    VISUAL = "vim";
  };

  environment.systemPackages = with pkgs; let
    git-crypt = pkgs.git-crypt.overrideAttrs (attrs: rec {
      worktreePatch = fetchurl {
        url = "https://github.com/AGWA/git-crypt/files/2771938/git-crypt-support-worktree-simple-version-patch.txt";
        sha256 = "1k477m6g3zjdarjr38lndh0kpgkp0yi8lg2iqdispfd4c85krrax";
      };
      patches = [ worktreePatch ];
    });
  in [
    clang
    dgit
    file
    git git-crypt gitAndTools.hub gitAndTools.lab
    gnupg
    less
    ncdu
    pass
    subversion
    tig
    tokei
    vim
    wget
  ];

  fonts = {
    fonts = [ pkgs.dejavu_nerdfont ];
    fontconfig.defaultFonts.monospace =
      [ "DejaVu Sans Mono Nerd Font Complete Mono" ];
  };

  documentation.dev.enable = true;

  programs.thefuck.enable = true;
  programs.firejail.enable = true;
  programs.mtr.enable = true;
}
