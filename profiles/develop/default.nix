{ pkgs, ... }:

{
  imports = [ ./fish ./tmux ./haskell ./python ./dotnet ./android ./javascript ./golang ];

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
    cfcli
    clang
    dstat
    exa
    execline
    file
    git
    htop
    iotop
    less
    lsof
    ltrace
    mercurial
    ncdu
    nethogs
    nix-diff
    nix-top
    nload
    page
    pass
    pijul
    psmisc
    s6
    s6-dns
    s6-linux-utils
    s6-linux-init
    s6-networking
    s6-portable-utils
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
