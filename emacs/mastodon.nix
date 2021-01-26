{ config, lib, usr, pkgs, domains, ... }:

{
  emacs-loader.mastodon = {
    demand = true;
    config = ''
      (setq mastodon-instance-url "https://u.${domains.srvc}")
    '';
  };
}
