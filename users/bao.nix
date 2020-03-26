{ config ? {}, pkgs, ... }: {
  imports = [
    ../profiles/develop
  ];

  users.users.bao = {
    uid = 1000;
    description = "default";
    shell = pkgs.fish;
    isNormalUser = true;
    extraGroups = [ "wheel" "adbusers" ];
  } // import ../secrets/user.password.nix;

  home-manager.users.bao = {
    imports = [
      ./shells/fish
      ./services/gnupg.nix
    ];

    programs.home-manager.enable = true;
    programs.fish.enable = true;

    programs.htop = {
      enable = true;

      headerMargin = false;
      meters = {
        left = [ "LeftCPUs2" "Memory" "Swap" "Hostname" ];
        right = [ "RightCPUs2" "Tasks" "LoadAverage" "Uptime" ];
      };

      treeView = false;
      showProgramPath = false;

      hideThreads = false;
      hideKernelThreads = false;
      hideUserlandThreads = true;
    };

    services.lorri.enable = true;
    services.gpg-agent.enable = true;

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
      };
    };
  };
}
