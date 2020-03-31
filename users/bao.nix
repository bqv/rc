{ config ? {}, pkgs, lib, ... }:

let
  ipfscat = pkgs.writeShellScriptBin "ipfscat" ''
    export IPFS_PATH='/var/lib/ipfs'
    bold="$(${pkgs.ncurses}/bin/tput bold)"
    sgr0="$(${pkgs.ncurses}/bin/tput sgr0)"
    ${pkgs.ipfs}/bin/ipfs add $@ |\
    ${pkgs.gnugrep}/bin/grep added |\
    ${pkgs.coreutils}/bin/cut -d' ' -f 2 |\
    ${pkgs.findutils}/bin/xargs -I{} echo "https://ipfs.io/ipfs/{}" |\
    ${pkgs.coreutils}/bin/tee >( \
    ${pkgs.xsel}/bin/xsel -i -p ) >( \
    ${pkgs.xsel}/bin/xsel -i -s ) >( \
    ${pkgs.xsel}/bin/xsel -i -b ) |\
    ${pkgs.findutils}/bin/xargs echo $bold"Copied:"$sgr0
  '';
in {
  imports = [
    ../profiles/develop
  ];

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
      ./editors/emacs
      ./utilities/htop
      ./services/gnupg.nix
    ];

    programs.home-manager.enable = true;
    programs.fish.enable = true;
    programs.htop.enable = true;

    services.lorri.enable = true;
    services.gpg-agent.enable = true;

    services.mpd = let
      inherit (config.home-manager.users.bao) xdg;
    in {
      enable = true;
      musicDirectory = xdg.userDirs.music;
    };

    home.packages = with pkgs; [
      abduco dvtm git yadm vim htop pstree fortune cowsay coreutils pv # Shell Essential
      nmap wget curl # Networking
      gnupg bitwarden-cli protonvpn-cli-ng git-crypt # Security
      file jq direnv ipfscat # Utility
      xsel xclip scrot # Utility
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
  };
}
