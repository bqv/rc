{ pkgs, ... }:

{
  imports = [ ./fish ./tmux ./haskell ./python ./dotnet ./android ];

  environment.shellAliases = {
 #  v = "$EDITOR";
  };

  environment.sessionVariables = {
    PAGER = "less";
    LESS = "-iFJMRWX -z-4 -x4";
    LESSOPEN = "|${pkgs.lesspipe}/bin/lesspipe.sh %s";
    EDITOR = "vim";
    VISUAL = "vim";
  };

  environment.systemPackages = with pkgs; [
    bat
    clang
    dstat
    exa
    execline
    file
    git
    htop
    iotop
    less
    ltrace
    mercurial
    ncdu
    nix-diff
    nix-top
    pass
    strace
    socat
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
