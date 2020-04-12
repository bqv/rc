{ lib, pkgs, ... }:

{
  users.defaultUserShell = pkgs.zsh;

  environment = {
    sessionVariables = {
      BAT_PAGER = "less";
    };

    shellAliases = {
      cat = "${pkgs.bat}/bin/bat";

      df = "df -h";
      du = "du -h";

      ls = "exa";
      l = "ls -lhg --git";
      la = "l -a";
      t = "l -T";
      ta = "la -T";

      ps = "${pkgs.procs}/bin/procs";
    };

    systemPackages = with pkgs; [
      any-nix-shell
      bat
      bzip2
      direnv
      exa
      gitAndTools.hub
      gzip
      lrzip
      p7zip
      procs
      skim
      unrar
      unzip
      xz
    ];
  };

  programs.fish = {
    enable = true;
    promptInit = ''
      any-nix-shell fish --info-right | source
    '';
  };
}
