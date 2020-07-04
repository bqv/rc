{ config, pkgs, lib, ... }:

{
  imports = [
    ../profiles/develop
  ];

  environment.variables = {
    GITHUB_TOKEN = (import ../secrets/git.github.nix).oauth-token;
  };

  services.dbus.packages = with pkgs; [ gnome3.dconf ];

  users.users.bao = {
    uid = 1000;
    group = "users";
    shell = pkgs.xonsh;
    isNormalUser = true;
    extraGroups = [ "wheel" "adbusers" "ipfs" ];
  } // import ../secrets/user.password.nix
    // import ../secrets/user.description.nix;

  home-manager.users.bao = let
    home-config = config.home-manager.users.bao;
  in {
    imports = [
      ./shells/fish
      ./shells/xonsh
      ./browsers/firefox
      ./company/locationextreme
      ./editors/emacs
      ./editors/vim
      ./media/gpodder
      ./media/spotify
      ./media/radio
      ./utilities/git
      ./utilities/darcs
      ./utilities/htop
      ./services/gnupg
      ./services/mpd
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
    programs.qutebrowser.enable = true;
    programs.firefox.enable = true;
    programs.xonsh.enable = true;
    programs.fish.enable = true;
    programs.htop.enable = true;
    programs.bat.enable = true;
    programs.fzf.enable = true;
    programs.tmux.enable = true;
    programs.emacs.enable = true;
    programs.neovim.enable = true;
    programs.jq.enable = true;
    programs.direnv.enable = true;
    programs.texlive.enable = true;
    programs.taskwarrior.enable = true;
    programs.neomutt.enable = true;
    programs.obs-studio.enable = true;
    programs.gpodder.enable = true;
    programs.mpv.enable = true;
    programs.feh.enable = true;
    programs.git.enable = true;
    programs.ssh.enable = false; # TODO

    services.lorri.enable = true;
    services.gpg-agent.enable = true;
    services.spotifyd.enable = true;
    services.mpd.enable = true;
    services.taskwarrior-sync.enable = true;
    services.dunst.enable = true;
    services.emacs.enable = true;
    services.syncthing.enable = false; # TODO
    services.unclutter.enable = false; # TODO

    home.packages = with pkgs; let
      emms-play-file = pkgs.writeScriptBin "emms-play-file" ''
        !#${pkgs.execline}/bin/execlineb -W
        ${home-config.programs.emacs.package}/bin/emacsclient --eval "(emms-play-file \"$@\")"
      '';
    in [
      abduco dvtm # Terminal Multiplexing
      yadm # Dotfile Management
      pstree # Process Monitoring
      pv pup # Pipe Management
      timewarrior # Time Management
      nmap wget curl ipfscat onionshare mitmproxy aria2 # Network Utilities
      bitwarden-cli protonvpn-cli-ng # Password Management
      file exa unrar unzip ncdu tree # File Management
      audacity # Audio Utilities
      xpra xsel xclip scrot # X11 Utilities
      gdb lldb radare2 radare2-cutter jadx # Debug Utilities
    ] ++ lib.optional home-config.programs.emacs.enable emms-play-file;

    home.file."mimeapps.list".force = lib.mkForce true;
    xdg = let
      inherit (home-config.home) homeDirectory;
    in rec {
      enable = true;

      cacheHome = "${homeDirectory}/.cache";
      configHome = "${homeDirectory}/.config";
      dataHome = "${homeDirectory}/.local/share";

      userDirs = {
        enable = true;

        desktop = "${dataHome}/desktop";
        documents = "${homeDirectory}/doc";
        download = "${homeDirectory}/tmp";
        music = "${homeDirectory}/var/music";
        pictures = "${homeDirectory}/var/images";
        publicShare = "${homeDirectory}/var/share";
        templates = "${configHome}/templates";
        videos = "${homeDirectory}/var/videos";
      };

      mimeApps = let
        firefox = "firefox.desktop";
        qutebrowser = "org.qutebrowser.qutebrowser.desktop";
        thunderbird = "thunderbird.desktop";
      in {
        enable = true;

        defaultApplications."text/html" = firefox;
        defaultApplications."x-scheme-handler/http" = firefox;
        defaultApplications."x-scheme-handler/https" = firefox;
        defaultApplications."x-scheme-handler/ftp" = firefox;
        defaultApplications."x-scheme-handler/chrome" = firefox;
        defaultApplications."application/x-extension-htm" = firefox;
        defaultApplications."application/x-extension-html" = firefox;
        defaultApplications."application/x-extension-shtml" = firefox;
        defaultApplications."application/xhtml+xml" = firefox;
        defaultApplications."application/x-extension-xhtml" = firefox;
        defaultApplications."application/x-extension-xht" = firefox;

        defaultApplications."x-scheme-handler/about" = qutebrowser;
        defaultApplications."x-scheme-handler/unknown" = qutebrowser;

        defaultApplications."x-scheme-handler/mailto" = thunderbird;
        defaultApplications."x-scheme-handler/news" = thunderbird;
        defaultApplications."x-scheme-handler/snews" = thunderbird;
        defaultApplications."x-scheme-handler/nntp" = thunderbird;
        defaultApplications."x-scheme-handler/feed" = thunderbird;
        defaultApplications."message/rfc822" = thunderbird;
        defaultApplications."application/rss+xml" = thunderbird;
        defaultApplications."application/x-extension-rss" = thunderbird;

        associations.added."x-scheme-handler/http" = [ firefox ];
        associations.added."x-scheme-handler/https" = [ firefox ];
        associations.added."x-scheme-handler/ftp" = [ firefox ];
        associations.added."x-scheme-handler/chrome" = [ firefox ];
        associations.added."text/html" = [ firefox ];
        associations.added."application/xhtml+xml" = [ firefox ];
        associations.added."application/x-extension-htm" = [ firefox ];
        associations.added."application/x-extension-html" = [ firefox ];
        associations.added."application/x-extension-shtml" = [ firefox ];
        associations.added."application/x-extension-xhtml" = [ firefox ];
        associations.added."application/x-extension-xht" = [ firefox ];

        associations.added."x-scheme-handler/mailto" = [ thunderbird ];
        associations.added."x-scheme-handler/news" = [ thunderbird ];
        associations.added."x-scheme-handler/snews" = [ thunderbird ];
        associations.added."x-scheme-handler/nntp" = [ thunderbird ];
        associations.added."x-scheme-handler/feed" = [ thunderbird ];
        associations.added."message/rfc822" = [ thunderbird ];
        associations.added."application/rss+xml" = [ thunderbird ];
        associations.added."application/x-extension-rss" = [ thunderbird ];
      };
    };

    gtk = {
      enable = true;
      theme = {
        name = "Adwaita-dark";
        package = pkgs.gnome-themes-extra;
      };
      iconTheme = {
        name = "Adwaita-dark";
        package = pkgs.gnome3.adwaita-icon-theme;
      };
      font = {
        name = "Cantarell 11";
        package = pkgs.cantarell-fonts;
      };
    };

    qt = {
      enable = true;
      platformTheme = "gnome";
    };
  };
}
