{ config ? {}, pkgs, lib, ... }:

let
  # TODO: Check for DISPLAY
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
in {
  imports = [
    ../profiles/develop
  ];

  services.dbus.packages = with pkgs; [ gnome3.dconf ];

  users.users.bao = {
    uid = 1000;
    description = "Tony";
    shell = pkgs.fish;
    isNormalUser = true;
    extraGroups = [ "wheel" "adbusers" ];
  } // import ../secrets/user.password.nix;

  home-manager.users.bao = {
    imports = [
      ./shells/fish
      ./browsers/firefox
      ./editors/emacs
      ./editors/vim
      ./media/gpodder
      ./media/spotify
      ./utilities/git
      ./utilities/htop
      ./services/gnupg
      ./services/mpd
    ];

    programs.home-manager.enable = true;
    programs.command-not-found.enable = true;
    programs.qutebrowser.enable = true;
    programs.firefox.enable = true;
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
    #programs.git.enable = true;
    #programs.ssh.enable = true;

    services.lorri.enable = true;
    services.gpg-agent.enable = true;
    services.spotifyd.enable = true;
    services.mpd.enable = true;
    services.taskwarrior-sync.enable = true;
    #services.dunst.enable = true;
    #services.emacs.enable = true;
    #services.syncthing.enable = true;
    #services.unclutter.enable = true;

    home.packages = with pkgs; [
      abduco dvtm # Terminal Multiplexing
      yadm # Dotfile Management
      pstree # Process Monitoring
      pv # Pipe Management
      nmap wget curl ipfscat mitmproxy # Network Utilities
      bitwarden-cli protonvpn-cli-ng # Password Management
      file exa unrar unzip ncdu # File Management
      xsel xclip scrot # X11 Utilities
      gdb lldb radare2 radare2-cutter # Debug Utilities
    ];

    home.file."mimeapps.list".force = lib.mkForce true;
    xdg = let
      inherit (config.home-manager.users.bao.home) homeDirectory;
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
