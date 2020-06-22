{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.org-jira = {
    demand = true;
    after = [ "org-mode" ];
  };
}
