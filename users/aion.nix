{ config, pkgs, lib, ... }:

{
  imports = [
    ./shells/fish
    ./shells/xonsh
    ./browsers/nyxt
    ./editors/vim
   #./media/radio
    ./media/mpv
   #./utilities/ssh
    ./utilities/git
    ./utilities/darcs
    ./utilities/htop
    ./services/gnupg
  ];

  home.file.".bashrc".text = ''
    # If not running interactively, don't do anything
    [[ $- != *i* ]] && return

    source /etc/profile

    [[ $INSIDE_EMACS == "vterm" ]] && [[ $IN_NIX_SHELL == "" ]] && exec xonsh

    PS1='[\u@\h \W]\$ '

    PS1="\n\[\e[1;30m\][''$$:$PPID - \j:\!\[\e[1;30m\]]\[\e[0;36m\] \T \[\e[1;30m\][\[\e[1;34m\]\u@\H\[\e[1;30m\]:\[\e[0;37m\]''${SSH_TTY:-o} \[\e[0;32m\]+''${SHLVL}\[\e[1;30m\]] \[\e[1;37m\]\w\[\e[0;37m\] \n\$ "
  '';
  home.file.".profile".text = ''
  '';
  home.file.".config/nixpkgs/config.nix".text = ''
    {
      ${lib.optionalString config.nixpkgs.config.allowUnfree "allowUnfree = true;"}
    }
  '';

  programs.home-manager.enable = true;
  programs.command-not-found.enable = false;
  programs.xonsh.enable = true;
  programs.fish.enable = true;
  programs.htop.enable = true;
  programs.bat.enable = true;
  programs.fzf.enable = true;
  programs.tmux.enable = true;
  programs.neovim.enable = true;
  programs.jq.enable = true;
  programs.direnv.enable = true;
  programs.mpv.enable = true;
  programs.feh.enable = true;
  programs.git.enable = true;
  programs.ssh.enable = true;

  services.lorri.enable = true;
  services.gpg-agent.enable = true;
  services.sxhkd.enable = true;

  #systemd.user.startServices = true; # broken by the [nix-env -> nix profile] move

  xsession.windowManager.bspwm.enable = true;

  home.packages = with pkgs; [
   #appimage-run # Package Tools
   #abduco dvtm # Terminal Multiplexing
   #pstree bottom # Process Monitoring
   #pv pup # Pipe Management
   #nmap wget curl mitmproxy aria2 # Network Utilities
   #ipfscat onionshare nyxt tuir redditgtk # Communication Tools
   #file exa unrar unzip ncdu tree # File Management
  ];
}
