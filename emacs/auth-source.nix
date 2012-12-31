{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.auth-source = {
    demand = true;
    name = "pass";
    config = ''
      (require 'auth-source-pass)
      (setq auth-sources '(;default
                           ;"secrets:session"
                           ;"secrets:Login"
                           password-store
                           "~/.authinfo"
                           "~/.authinfo.gpg"))
    '';
    systemDeps = with pkgs; [ pass ];
  };
}
