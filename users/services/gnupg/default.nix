{ config ? {}, lib, pkgs, ... }:

with lib; let
  cfg = config.services.gpg-agent;
in {
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      gnupg 
    ];

    home.extraProfileCommands = ''
      export GPG_TTY=$(tty)
      if [[ -n "$SSH_CONNECTION" ]] ;then
        export PINENTRY_USER_DATA="USE_CURSES=1"
      fi
    '';

    programs.gpg.enable = true;

    services.gpg-agent = {
      defaultCacheTtl = 600;
      defaultCacheTtlSsh = 0;
      maxCacheTtl = 7200;
      enableExtraSocket = true;
      enableSshSupport = true;
      sshKeys = [ "C425D701DBB41091CAC74AB2A7476FC5237EDBC7" ];
      extraConfig = ''
        allow-emacs-pinentry
        allow-preset-passphrase
      '';
      verbose = true;
    };
  };
}
