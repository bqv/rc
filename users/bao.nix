{ config ? {}, pkgs, ... }:

{
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
      file jq direnv # Utility
      netsurf.browser # Utility
    ];

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

      mimeApps = {
        enable = true;
        #[Default Applications]
        #text/html=firefox.desktop
        #x-scheme-handler/http=firefox.desktop
        #x-scheme-handler/https=firefox.desktop
        #x-scheme-handler/about=org.qutebrowser.qutebrowser.desktop
        #x-scheme-handler/unknown=org.qutebrowser.qutebrowser.desktop
        #x-scheme-handler/mailto=userapp-Daily-YZ3MH0.desktop
        #message/rfc822=userapp-Daily-YZ3MH0.desktop
        #x-scheme-handler/news=userapp-Thunderbird-6VWVYZ.desktop
        #x-scheme-handler/snews=userapp-Thunderbird-6VWVYZ.desktop
        #x-scheme-handler/nntp=userapp-Thunderbird-6VWVYZ.desktop
        #x-scheme-handler/feed=userapp-Thunderbird-EVKWYZ.desktop
        #application/rss+xml=userapp-Thunderbird-EVKWYZ.desktop
        #application/x-extension-rss=userapp-Thunderbird-EVKWYZ.desktop
        #x-scheme-handler/ftp=firefox.desktop
        #x-scheme-handler/chrome=firefox.desktop
        #application/x-extension-htm=firefox.desktop
        #application/x-extension-html=firefox.desktop
        #application/x-extension-shtml=firefox.desktop
        #application/xhtml+xml=firefox.desktop
        #application/x-extension-xhtml=firefox.desktop
        #application/x-extension-xht=firefox.desktop
        #[Added Associations]
        #x-scheme-handler/mailto=userapp-Thunderbird-TYK1YZ.desktop;userapp-Daily-YZ3MH0.desktop;
        #message/rfc822=userapp-Thunderbird-TYK1YZ.desktop;userapp-Daily-YZ3MH0.desktop;
        #x-scheme-handler/news=userapp-Thunderbird-6VWVYZ.desktop;
        #x-scheme-handler/snews=userapp-Thunderbird-6VWVYZ.desktop;
        #x-scheme-handler/nntp=userapp-Thunderbird-6VWVYZ.desktop;
        #x-scheme-handler/feed=userapp-Thunderbird-EVKWYZ.desktop;
        #application/rss+xml=userapp-Thunderbird-EVKWYZ.desktop;
        #application/x-extension-rss=userapp-Thunderbird-EVKWYZ.desktop;
        #x-scheme-handler/http=firefox.desktop;
        #x-scheme-handler/https=firefox.desktop;
        #x-scheme-handler/ftp=firefox.desktop;
        #x-scheme-handler/chrome=firefox.desktop;
        #text/html=firefox.desktop;
        #application/x-extension-htm=firefox.desktop;
        #application/x-extension-html=firefox.desktop;
        #application/x-extension-shtml=firefox.desktop;
        #application/xhtml+xml=firefox.desktop;
        #application/x-extension-xhtml=firefox.desktop;
        #application/x-extension-xht=firefox.desktop;
      };
    };
  };
}
