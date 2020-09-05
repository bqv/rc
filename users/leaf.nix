{ config, pkgs, lib, ... }:

{
  imports = [
    ../profiles/develop
  ];

  environment.variables = {
    GITHUB_TOKEN = (import ../secrets/git.github.nix).oauth-token;
  };

  services.dbus.packages = with pkgs; [ gnome3.dconf ];

  users.users.leaf = {
    uid = 1000;
    group = "users";
    shell = pkgs.xonsh;
    isNormalUser = true;
    extraGroups = [ "wheel" "adbusers" ];
  } // import ../secrets/leaf.password.nix;

  home-manager.users.leaf = let
    home-config = config.home-manager.users.leaf;
  in {
    imports = [
      ./shells/fish
      ./shells/xonsh
      ./editors/vim
      ./utilities/git
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

    programs.home-manager.enable = true;
    programs.command-not-found.enable = false; # Needs flake-safe version
    programs.xonsh.enable = true;
    programs.fish.enable = true;
    programs.htop.enable = true;
    programs.bat.enable = true;
    programs.fzf.enable = true;
    programs.tmux.enable = true;
    programs.neovim.enable = true;
    programs.jq.enable = true;
    programs.direnv.enable = true;
    programs.taskwarrior.enable = true;
    programs.mpv.enable = true;
    programs.feh.enable = true;
    programs.git.enable = true;
    programs.ssh.enable = false; # TODO

    services.lorri.enable = true;
    services.gpg-agent.enable = true;
    services.spotifyd.enable = true;
    services.taskwarrior-sync.enable = true;
    services.syncthing.enable = false; # TODO
    services.unclutter.enable = false; # TODO

    home.packages = with pkgs; let
      ipfscat = pkgs.writeShellScriptBin "ipfscat" ''
        export IPFS_PATH='/var/lib/ipfs'
        bold="$(${pkgs.ncurses}/bin/tput bold)"
        sgr0="$(${pkgs.ncurses}/bin/tput sgr0)"
        if [ -z "$DISPLAY" ]; then
        ${pkgs.ipfs}/bin/ipfs add $@ |\
        ${pkgs.gnugrep}/bin/grep added |\
        ${pkgs.coreutils}/bin/cut -d' ' -f 2 |\
        ${pkgs.findutils}/bin/xargs -I{} echo "https://gateway.ipfs.io/ipfs/{}" |\
        ${pkgs.findutils}/bin/xargs echo $bold"Copy:"$sgr0
        else
        ${pkgs.ipfs}/bin/ipfs add $@ |\
        ${pkgs.gnugrep}/bin/grep added |\
        ${pkgs.coreutils}/bin/cut -d' ' -f 2 |\
        ${pkgs.findutils}/bin/xargs -I{} echo "https://gateway.ipfs.io/ipfs/{}" |\
        ${pkgs.xclip}/bin/xclip -i -r -f -selection primary |\
        ${pkgs.xclip}/bin/xclip -i -r -f -selection secondary |\
        ${pkgs.xclip}/bin/xclip -i -r -f -selection clipboard |\
        ${pkgs.findutils}/bin/xargs echo $bold"Copied:"$sgr0
        fi
      '';
    in [
      abduco dvtm # Terminal Multiplexing
      yadm # Dotfile Management
      pstree # Process Monitoring
      pv pup # Pipe Management
      nmap wget curl ipfscat mitmproxy # Network Utilities
      file exa unrar unzip ncdu # File Management
      xpra xsel xclip scrot # X11 Utilities
      gdb lldb radare2 radare2-cutter # Debug Utilities
    ];

  };
}
