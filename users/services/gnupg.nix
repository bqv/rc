{ config ? {}, lib, pkgs, ... }:

with lib; let
  cfg = config.services.gpg-agent;
in {
  config = mkIf cfg.enable {
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
